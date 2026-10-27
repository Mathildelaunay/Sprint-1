```python
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor


# SETTINGS


DATA_FOLDER = Path.cwd()

TRAIN_SEASONS = ["14_15","15_16","16_17","17_18","18_19","19_20","20_21","21_22","22_23","23_24","24_25"]

TEST_SEASON = "25_26"
CURRENT_SEASON = "26_27"

CHECKPOINTS = [6,10,15,20,25]

FEATURES = ["Matches_Played","GF_So_Far","GA_So_Far","Early_GF_Per_Match","Early_GA_Per_Match","Previous_GF_Per_Match","Previous_GA_Per_Match","Previous_Goal_Ratio"]


# LOAD MATCH DATA


def load_matches(season):
    file_path = DATA_FOLDER / f"{season}.csv"
    if not file_path.exists():
        raise FileNotFoundError(f"Could not find {file_path}. "
                    "Make sure your CSV files are in the current folder.")
    df = pd.read_csv(file_path)
    required_columns = ["HomeTeam", "AwayTeam", "FTHG", "FTAG"]
    missing = [col for col in required_columns if col not in df.columns]
    if missing:
        raise ValueError(f"{season}.csv is missing these columns: {missing}")

    # Keep only matches with completed scores
    df["FTHG"] = pd.to_numeric(df["FTHG"], errors="coerce")
    df["FTAG"] = pd.to_numeric(df["FTAG"], errors="coerce")
    df = df.dropna(subset=required_columns).copy()

    # Sort chronologically when usable dates are available
    if "Date" in df.columns:
        parsed_dates = pd.to_datetime(df["Date"], dayfirst=True, errors="coerce")
        if parsed_dates.notna().any():
            df["_ParsedDate"] = parsed_dates
            df = df.sort_values("_ParsedDate",kind="stable",na_position="last")
            df = df.drop(columns="_ParsedDate")

    return df.reset_index(drop=True)


# GET A TEAM'S MATCHES


def get_team_matches(df, team):
    home = df[df["HomeTeam"] == team].copy()
    home["GF"] = home["FTHG"]
    home["GA"] = home["FTAG"]

    away = df[df["AwayTeam"] == team].copy()
    away["GF"] = away["FTAG"]
    away["GA"] = away["FTHG"]

    # Preserve the original order of matches
    matches = pd.concat([home, away], axis=0).sort_index()
    return matches.reset_index(drop=True)


# CALCULATE FINAL SEASON TOTALS


def get_season_totals(df):
    teams = sorted(set(df["HomeTeam"].dropna()) | set(df["AwayTeam"].dropna()))
    totals = {}
    for team in teams:
        matches = get_team_matches(df, team)
        totals[team] = {"Matches_Played": len(matches),"GF": float(matches["GF"].sum()),"GA": float(matches["GA"].sum())}
    return totals



# PREVIOUS SEASON FEATURES


def get_previous_season_features(previous_totals, team):
    # Defaults for teams without previous-season data
    default = {"Previous_GF_Per_Match": 1.35,"Previous_GA_Per_Match": 1.35,"Previous_Goal_Ratio": 1.0}

    if previous_totals is None or team not in previous_totals:
        return default.copy()
    stats = previous_totals[team]
    played = stats["Matches_Played"]

    if played == 0:
        return default.copy()

    previous_gf_rate = stats["GF"] / played
    previous_ga_rate = stats["GA"] / played

    if stats["GA"] > 0:
        previous_ratio = stats["GF"] / stats["GA"]
    elif stats["GF"] > 0:
        previous_ratio = stats["GF"]
    else:
        previous_ratio = 1.0

    return {"Previous_GF_Per_Match": previous_gf_rate,"Previous_GA_Per_Match": previous_ga_rate,"Previous_Goal_Ratio": previous_ratio}


# CREATE FEATURES FROM A TEAM'S FIRST N MATCHES


def create_features(team_matches, previous_features):
    matches_played = len(team_matches)

    gf_so_far = float(team_matches["GF"].sum())
    ga_so_far = float(team_matches["GA"].sum())

    if matches_played > 0:
        early_gf_rate = gf_so_far / matches_played
        early_ga_rate = ga_so_far / matches_played
    else:
        early_gf_rate = 0.0
        early_ga_rate = 0.0

    return {"Matches_Played": matches_played,"GF_So_Far": gf_so_far,"GA_So_Far": ga_so_far,"Early_GF_Per_Match": early_gf_rate,"Early_GA_Per_Match": early_ga_rate,**previous_features}


# BUILD TRAINING DATA: 2014/15 TO 2024/25


training_rows = []

print("Loading historical seasons and building training data...")

# Load each historical season and calculate its totals once
historical_data = {}
historical_totals = {}

for season in TRAIN_SEASONS:
    historical_data[season] = load_matches(season)
    historical_totals[season] = get_season_totals(historical_data[season])

for season_index, season in enumerate(TRAIN_SEASONS):
    season_df = historical_data[season]
    final_totals = historical_totals[season]

    if season_index > 0:
        previous_season = TRAIN_SEASONS[season_index - 1]
        previous_totals = historical_totals[previous_season]
    else:
        previous_totals = None

    for team in sorted(final_totals.keys()):
        team_matches = get_team_matches(season_df, team)
        final_stats = final_totals[team]

        for checkpoint in CHECKPOINTS:
            if len(team_matches) < checkpoint:
                continue

            early_matches = team_matches.iloc[:checkpoint]
            previous_features = get_previous_season_features(previous_totals, team)
            features = create_features(early_matches, previous_features)
            training_rows.append({**features,"Target_GF": final_stats["GF"],"Target_GA": final_stats["GA"]})

training_df = pd.DataFrame(training_rows)

if training_df.empty:
    raise ValueError("No training data was created. Check your CSV files.")

X_train = training_df[FEATURES]
y_train_gf = training_df["Target_GF"]
y_train_ga = training_df["Target_GA"]

print(f"Training examples: {len(training_df)}")


# TRAIN THE RANDOM FOREST MODELS


model_gf = RandomForestRegressor(n_estimators=400,max_depth=6,min_samples_leaf=3,random_state=42,n_jobs=-1)

model_ga = RandomForestRegressor(n_estimators=400,max_depth=6,min_samples_leaf=3,random_state=42,n_jobs=-1)

model_gf.fit(X_train, y_train_gf)
model_ga.fit(X_train, y_train_ga)

print("Both models trained successfully.")


# PREDICT A SEASON FROM EACH TEAM'S FIRST SIX MATCHES


def predict_season(season, previous_season):
    season_df = load_matches(season)
    previous_df = load_matches(previous_season)
    previous_totals = get_season_totals(previous_df)
    teams = sorted(set(season_df["HomeTeam"].dropna()) | set(season_df["AwayTeam"].dropna()))
    predictions = []

    for team in teams:
        team_matches = get_team_matches(season_df,team)

        # Only predict teams with at least six completed matches
        if len(team_matches) < 6:
            continue

        first_six = team_matches.iloc[:6]

        previous_features = get_previous_season_features(previous_totals,team)

        features = create_features(first_six,previous_features)

        X_team = pd.DataFrame([features])[FEATURES]

        predicted_gf = max(0.0, float(model_gf.predict(X_team)[0]))

        predicted_ga = max(0.0, float(model_ga.predict(X_team)[0]))

        if predicted_ga > 0:
            predicted_ratio = predicted_gf / predicted_ga
        elif predicted_gf > 0:
            predicted_ratio = np.inf
        else:
            predicted_ratio = 0.0

        predictions.append({"Team": team,"Matches_Played": len(first_six),"GF_So_Far": float(first_six["GF"].sum()),"GA_So_Far": float(first_six["GA"].sum()),"Predicted_GF_Total": predicted_gf,"Predicted_GA_Total": predicted_ga,"Predicted_Goal_Ratio": predicted_ratio})

    result = pd.DataFrame(predictions)

    if result.empty:
        raise ValueError(f"No teams had at least six completed matches in "f"{season}.csv.")

    result = result.sort_values("Predicted_Goal_Ratio",ascending=False,kind="stable").reset_index(drop=True)

    result.insert(0,"Predicted_Rank",range(1,len(result) + 1))

    return result


# TEST THE MODEL ON 2025/26


print("\nPredicting 2025/26 from each team's first six matches...")

test_predictions = predict_season(TEST_SEASON,"24_25")

# Calculate actual final results for evaluation
test_df = load_matches(TEST_SEASON)
actual_totals = get_season_totals(test_df)

actual_rows = []

for team, stats in actual_totals.items():
    actual_gf = stats["GF"]
    actual_ga = stats["GA"]

    if actual_ga > 0:
        actual_ratio = actual_gf / actual_ga
    elif actual_gf > 0:
        actual_ratio = np.inf
    else:
        actual_ratio = 0.0

    actual_rows.append({"Team": team,"Actual_GF_Total": actual_gf,"Actual_GA_Total": actual_ga,"Actual_Goal_Ratio": actual_ratio})

actual_df = pd.DataFrame(actual_rows)

actual_df = actual_df.sort_values("Actual_Goal_Ratio",ascending=False,kind="stable").reset_index(drop=True)

actual_df.insert(0,"Actual_Rank",range(1,len(actual_df) + 1))

# Match predicted and actual results by team name
comparison = test_predictions.merge(actual_df,on="Team",how="inner")

# Calculate the absolute difference between predicted and actual ranks
comparison["Absolute_Rank_Error"] = (comparison["Predicted_Rank"] -comparison["Actual_Rank"]).abs()


# DISPLAY THE FOUR-COLUMN TEST TABLE


comparison_display = comparison[["Team","Predicted_Rank","Predicted_Goal_Ratio","Absolute_Rank_Error"]].copy()

comparison_display = comparison_display.rename(columns={"Predicted_Rank": "Predicted_Ranking","Predicted_Goal_Ratio": "Predicted_Goals_Ratio"})

comparison_display["Predicted_Goals_Ratio"] = (comparison_display["Predicted_Goals_Ratio"].round(3))

comparison_display = comparison_display.sort_values("Predicted_Ranking").reset_index(drop=True)

test_output = (DATA_FOLDER / "test_25_26_predicted_vs_actual_rankings.csv")

comparison_display.to_csv(test_output, index=False)

print("\n" + "=" * 80)
print("2025/26 TEST: PREDICTED RANKINGS")
print("Rank error is measured against the actual final goal-ratio ranking.")
print("=" * 80)

print(comparison_display.to_string(index=False))


# 2025/26 TEST SUMMARY


number_of_teams = len(comparison)

mean_absolute_rank_error = (comparison["Absolute_Rank_Error"].mean())

exact_rank_matches = (comparison["Absolute_Rank_Error"] == 0).sum()

# Includes teams with an error of 0 or 1 place
within_1_position = (comparison["Absolute_Rank_Error"] <= 1).sum()

# Includes teams with an error of 0, 1, or 2 places
within_2_positions = (comparison["Absolute_Rank_Error"] <= 2).sum()

# Includes teams with an error of over 5 places
over_5_positions = (comparison["Absolute_Rank_Error"] > 5).sum()

print("\nTEST SUMMARY")
print("-" * 45)
print(f"Teams compared: {number_of_teams}")

print(f"Mean absolute ranking error: "f"{mean_absolute_rank_error:.2f} places")
print(f"Teams in exactly the correct position: "f"{exact_rank_matches} / {number_of_teams}")
print(f"Teams within at least 1 position: "f"{within_1_position} / {number_of_teams}")
print(f"Teams within at least 2 positions: "f"{within_2_positions} / {number_of_teams}")
print(f"Teams over 5 places out of position: "f"{over_5_positions} / {number_of_teams}")

print(f"\nSaved test comparison to: {test_output.name}")


# PREDICT THE CURRENT 2026/27 SEASON


print("\nPredicting the current 2026/27 season...")

current_predictions = predict_season(CURRENT_SEASON,TEST_SEASON)


# SAVE 2026/27 TEAM STATISTICS IN ALPHABETICAL ORDER


statistics_columns = ["Team","Matches_Played","GF_So_Far","GA_So_Far","Predicted_GF_Total","Predicted_GA_Total"]

statistics_table = current_predictions[statistics_columns].copy()

for col in ["GF_So_Far","GA_So_Far","Predicted_GF_Total","Predicted_GA_Total"]:
    statistics_table[col] = statistics_table[col].round(2)

# Sort alphabetically by team name
statistics_table = statistics_table.sort_values("Team",key=lambda names: names.str.casefold()).reset_index(drop=True)

statistics_file = (DATA_FOLDER / "predicted_26_27_team_statistics.csv")

statistics_table.to_csv(statistics_file, index=False)

print("\n" + "=" * 80)
print("2026/27 PREDICTED TEAM STATISTICS (ALPHABETICAL ORDER)")
print("=" * 80)

print(statistics_table.to_string(index=False))


# SAVE 2026/27 PREDICTED RANKINGS BY GOAL RATIO


rankings_table = current_predictions[["Predicted_Rank","Team","Predicted_Goal_Ratio"]].copy()

rankings_table["Predicted_Goal_Ratio"] = (rankings_table["Predicted_Goal_Ratio"].round(3))

rankings_file = DATA_FOLDER / "predicted_26_27_rankings.csv"

rankings_table.to_csv(rankings_file, index=False)

print("\n" + "=" * 80)
print("2026/27 PREDICTED RANKINGS BY GOAL RATIO")
print("=" * 80)

print(rankings_table.to_string(index=False))


# FINAL OUTPUT FILES


print("\nAll three CSV files have been saved:")
print(f"1. {test_output.name}")
print(f"2. {statistics_file.name}")
print(f"3. {rankings_file.name}")
```

    Loading historical seasons and building training data...
    Training examples: 1100
    Both models trained successfully.
    
    Predicting 2025/26 from each team's first six matches...
    
    ================================================================================
    2025/26 TEST: PREDICTED RANKINGS
    Rank error is measured against the actual final goal-ratio ranking.
    ================================================================================
              Team  Predicted_Ranking  Predicted_Goals_Ratio  Absolute_Rank_Error
           Arsenal                  1                  2.626                    0
         Liverpool                  2                  1.844                    2
          Man City                  3                  1.641                    1
         Tottenham                  4                  1.428                   12
           Chelsea                  5                  1.332                    2
         Newcastle                  6                  1.229                    4
       Bournemouth                  7                  1.176                    1
          Brighton                  8                  1.101                    2
       Aston Villa                  9                  1.094                    4
    Crystal Palace                 10                  1.087                    7
            Fulham                 11                  1.018                    2
         Brentford                 12                  0.969                    3
        Sunderland                 13                  0.965                    2
           Everton                 14                  0.929                    2
     Nott'm Forest                 15                  0.904                    4
            Wolves                 16                  0.740                    4
        Man United                 17                  0.714                   14
             Leeds                 18                  0.688                    4
          West Ham                 19                  0.655                    1
           Burnley                 20                  0.507                    1
    
    TEST SUMMARY
    ---------------------------------------------
    Teams compared: 20
    Mean absolute ranking error: 3.60 places
    Teams in exactly the correct position: 1 / 20
    Teams within at least 1 position: 5 / 20
    Teams within at least 2 positions: 11 / 20
    Teams over 5 places out of position: 3 / 20
    
    Saved test comparison to: test_25_26_predicted_vs_actual_rankings.csv
    
    Predicting the current 2026/27 season...
    
    ================================================================================
    2026/27 PREDICTED TEAM STATISTICS (ALPHABETICAL ORDER)
    ================================================================================
              Team  Matches_Played  GF_So_Far  GA_So_Far  Predicted_GF_Total  Predicted_GA_Total
           Arsenal               6       10.0        5.0               69.33               37.78
       Aston Villa               6        6.0       11.0               50.88               60.68
       Bournemouth               6        7.0       13.0               50.99               71.46
         Brentford               6       12.0        6.0               66.77               46.40
          Brighton               6       18.0        5.0               75.28               40.60
           Chelsea               6       15.0       13.0               69.67               57.87
          Coventry               6        5.0       11.0               36.55               64.41
    Crystal Palace               6        9.0       12.0               42.78               58.70
           Everton               6        8.0        5.0               49.42               49.50
            Fulham               6        6.0       10.0               39.79               59.69
              Hull               6        8.0        6.0               48.29               46.88
           Ipswich               6        9.0       12.0               41.73               65.00
             Leeds               6        8.0        5.0               50.35               46.78
         Liverpool               6        7.0        7.0               51.71               49.34
          Man City               6       16.0        5.0               87.34               36.95
        Man United               6        9.0        9.0               60.16               52.54
         Newcastle               6       10.0       13.0               52.64               58.24
     Nott'm Forest               6        5.0        8.0               38.16               53.34
        Sunderland               6        6.0       12.0               40.48               59.84
         Tottenham               6        3.0        9.0               33.77               56.65
    
    ================================================================================
    2026/27 PREDICTED RANKINGS BY GOAL RATIO
    ================================================================================
     Predicted_Rank           Team  Predicted_Goal_Ratio
                  1       Man City                 2.364
                  2       Brighton                 1.854
                  3        Arsenal                 1.835
                  4      Brentford                 1.439
                  5        Chelsea                 1.204
                  6     Man United                 1.145
                  7          Leeds                 1.076
                  8      Liverpool                 1.048
                  9           Hull                 1.030
                 10        Everton                 0.998
                 11      Newcastle                 0.904
                 12    Aston Villa                 0.839
                 13 Crystal Palace                 0.729
                 14  Nott'm Forest                 0.715
                 15    Bournemouth                 0.713
                 16     Sunderland                 0.677
                 17         Fulham                 0.667
                 18        Ipswich                 0.642
                 19      Tottenham                 0.596
                 20       Coventry                 0.567
    
    All three CSV files have been saved:
    1. test_25_26_predicted_vs_actual_rankings.csv
    2. predicted_26_27_team_statistics.csv
    3. predicted_26_27_rankings.csv



```python

```
