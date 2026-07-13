# materials

Experiment stimuli and task code for all eight studies.
Experiments were built in jsPsych 7.3 and hosted on Cognition.run.

## Contents

| File / Folder             | Description                                              |
|---------------------------|----------------------------------------------------------|
| `rules_task/`             | jsPsych code for the main rule enforcement task          |
| `stroop_task/`            | jsPsych code for the Stroop task (Studies 2, 3b, 4a, 4b)|
| `flanker_task/`           | jsPsych code for the Flanker task (Studies 2, 3b, 4a, 4b)|
| `scenarios.csv`           | All 8 rule scenarios with text, purpose, and 12 trial items per scenario |
| `practice_scenario.csv`   | Practice scenario (noise prohibition) used across studies|

## Rule scenarios

Each scenario consists of:
- A rule text (e.g., "No one may wear shoes in the house")
- A purpose statement (e.g., "To keep her house clean, Mary announced...")
- 12 behavioral items: 3 per condition (violation, compliance, literal violation, literal compliance)

## Task parameters

| Parameter              | Value                             |
|------------------------|-----------------------------------|
| Response keys          | E (left) / I (right), counterbalanced |
| Time limit (default)   | 8 seconds                         |
| Time limit (speed)     | 5 seconds (Study 3a)              |
| Time limit (accuracy)  | 10 seconds (Study 3a)             |
| Fixation ITI           | 250–2,000 ms (uniform random)     |
| Platform               | Cognition.run                     |
| Recruitment            | Prolific (native English speakers) |

## Preregistration

All eight studies were preregistered at:
https://researchbox.org/3227&PEER_REVIEW_passcode=AHEJAU
