Genomedir=/Database/hg38;
Genome=GRCh38;

sample="SRR1779331"
for s in $sample
do
samtools view -H $s.uniq.nodup.bam | sed -e 's/SN:\([0-9XY]\)/SN:chr\1/' -e 's/SN:MT/SN:chrM/' | samtools reheader - $s.uniq.nodup.bam > $s.uniq.nodup.chr.bam
samtools index $s.uniq.nodup.chr.bam
nohup bamCoverage -b $s.uniq.nodup.chr.bam -bs 1000 -o $s.uniq.nodup.1000.bw -p 10 &
nohup bamCoverage -b $s.uniq.nodup.chr.bam -bs 1000000 -o $s.uniq.nodup.1000000.bw -p 10 &
nohup bamCoverage -b $s.uniq.nodup.chr.bam -bs 10000 -o $s.uniq.nodup.10000.bw -p 10 &
nohup bamCoverage -b $s.uniq.nodup.chr.bam -bs 100000 -o $s.uniq.nodup.100000.bw -p 10 &
nohup bamCoverage -b $s.uniq.nodup.chr.bam -bl $Genomedir/$Genome.blacklist.bed -bs 1000000 -o $s.uniq.nodup.1000000rmbl.bw -p 10 &
nohup bamCoverage -b $s.uniq.nodup.chr.bam -bl $Genomedir/$Genome.blacklist.bed -bs 500000 -o $s.uniq.nodup.500000rmbl.bw -p 10 &
done