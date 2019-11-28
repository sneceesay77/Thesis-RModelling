# RModelling Data Flow With Cycle

#### We have implemented a comphrensive REST API that users can use to see how long it will take to run any of the six Machine learning applications included in this work. The following applications are included.

##### Support Vector Machines
##### Random Forest
##### Linear Regression
##### Logistic Regression
##### K-means
##### Naive Bayes

To test any of the algorithms below use the following parameters

##### DataSizeGB -> Size of data to process
##### NumEx -> Number of Executors
##### ExCore -> Executor Cores
##### ExMem -> Executor Memory
##### LevelPar -> Level of Parallelism

#### Examples

##### Support Vector Machines
```bash
curl -X POST -d '{"DataSizeGB":10, "NumEx":16, "ExCore":1, "ExMem":1, "LevelPar":6, "App":"SVM"}' -H 'Content-Type: application/json' https://sc306.host.cs.st-andrews.ac.uk/dfwc/
```
##### Random Forest
```bash
curl -X POST -d '{"DataSizeGB":10, "NumEx":16, "ExCore":1, "ExMem":1, "LevelPar":6, "App":"RF"}' -H 'Content-Type: application/json' https://sc306.host.cs.st-andrews.ac.uk/dfwc/
```
##### Linear Regression
```bash
curl -X POST -d '{"DataSizeGB":10, "NumEx":16, "ExCore":1, "ExMem":1, "LevelPar":6, "App":"LINEAR"}' -H 'Content-Type: application/json' https://sc306.host.cs.st-andrews.ac.uk/dfwc/
```
##### Logistic Regression
```bash
curl -X POST -d '{"DataSizeGB":10, "NumEx":16, "ExCore":1, "ExMem":1, "LevelPar":6, "App":"LR"}' -H 'Content-Type: application/json' https://sc306.host.cs.st-andrews.ac.uk/dfwc/
```
##### K-means
```bash
curl -X POST -d '{"DataSizeGB":10, "NumEx":16, "ExCore":1, "ExMem":1, "LevelPar":6, "App":"KMEANS"}' -H 'Content-Type: application/json' https://sc306.host.cs.st-andrews.ac.uk/dfwc/
```
##### Naive Bayes
```bash
curl -X POST -d '{"DataSizeGB":10, "NumEx":16, "ExCore":1, "ExMem":1, "LevelPar":6, "App":"BAYES"}' -H 'Content-Type: application/json' https://sc306.host.cs.st-andrews.ac.uk/dfwc/
```
