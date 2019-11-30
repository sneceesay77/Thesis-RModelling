#!/bin/bash
app=(RF KMEANS SVM BAYES LR LINEAR)
for h in "${app[@]}"
do
echo -e "DataSize\tNumex\tExMem\tExCore\tLevelPar\tPredictions" >> $h.txt
data=(1 2 4 6 8 12)
for i in "${data[@]}"
 do
  nex=(4 8 16 24 32)
   for j in "${nex[@]}"
     do
	 exm=(2 4 6 8)
	 for k in "${exm[@]}"
	 do
            nCores=(2 4 6 8)
	    for l in "${nCores[@]}"
            do
	      lp=(8 16 32 64)
	      for m in "${lp[@]}"
	      do
		  result=$(curl -X POST -d '{"DataSizeGB":'"$i"', "NumEx":'"$j"', "ExCore":'"$l"', "ExMem":'"$k"', "LevelPar":'"$m"', "App":'\"$h\"'}' -H \'Content-Type: application/json\' https://sc306.host.cs.st-andrews.ac.uk/dfwc/)
		  #echo $result	  
		  fresult=$(echo $result | jq --compact-output '.svm' | grep -Eoh '[0-9]*\.[0-9]*')
		  echo -e "$i\t$j\t$k\t$l\t$m\t$fresult" >> $h.txt
	      done
	    done	    
	done
   done
done
done
