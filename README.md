# RModelling Data Flow With Cycle

We have developed a simple proof of concept model and deployed it using R-Plumber on our server. Using our framework, you can test how long it will take to run a sample SVM algorithm implemented in the HiBench benchmarking suite using the following command.

curl -X POST -d '{"DataSizeGB":10, "NumEx":16, "ExCore":1, "ExMem":1, "LevelPar":6}' -H 'Content-Type: application/json' https://sc306.host.cs.st-andrews.ac.uk/dfwc/


You can change the JSON data to whatever you want. However, this is model based on our cluster and there are some limitation as to how much the values can be. 

