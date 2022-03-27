random_seed="1 2 3 4 5 6 7 8 9 10"
for r in $random_seed
do
mkdir random_seed${r}
percentage="0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.01 0.001"
for p in $percentage
do
sample="12724 12736 12742 12745 12750"
for s in $sample
do
nohup java -jar /Ubin/bin/picard.jar DownsampleSam I=./$s.uniq.nodup.bam O=random_seed${r}/$s.$p.uniq.nodup.bam P=$p R=$r &
done
done
done
