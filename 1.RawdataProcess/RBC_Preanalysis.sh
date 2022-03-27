Genome=GRCh38;
Genomedir=/Database/hg38;

sample="GLPExp_20g GLPExp_5r GLPExp_20r"
split=500000;
bedtools makewindows -g $Genomedir/$Genome.genome -w $split|sort -k1,1 -k2,2n> window${split}.bed
for s in $sample
do
    ln -s ../$s*fastq .
    cutadapt -f fastq -n 1 -e 0.1 -O 2 -m 16 -l 70 -U 1 -u 1 -a CTGTCTCTTATACACATCTCC -A CTGTCTCTTATACACATCTG  -o $s.1.fastq.clipper -p $s.2.fastq.clipper $s.1.fastq $s.2.fastq > $s.log
    wait
    bwa mem -t 90 $Genomedir/$Genome $s.1.fastq.clipper $s.2.fastq.clipper > $s.bwa.raw.sam
    wait
    python3.7 Sam.SoftClip.filter.py $s
    wait
    #Fifter four types reads(read unmapped、not primary alignment、read is PCR or optical duplicate、supplementary alignment)
    samtools view -Sb -F 3332 $s.bwa.sam -@ 90|samtools sort - -o $s.bam -@ 90    wait
    #Select reads which have uniq site mapping to reference genome
    samtools view -h $s.bam | grep -v -e 'XA:Z:' -e 'SA:Z:' | samtools view -b -@ 90|samtools sort - -o $s.uniq.bam -@ 90
    #Delete duplicate reads for uniq.bam
    java -jar $Softbin/picard.jar MarkDuplicates INPUT=${s}.uniq.bam OUTPUT=${s}.uniq.nodup.bam METRICS_FILE=${s}.dup ASSUME_SORTED=TRUE REMOVE_DUPLICATES=true
    samtools index $s.uniq.nodup.bam
    wait
    #Visulize reads count
    igvtools count -z 10 -w 5 $s.uniq.nodup.bam $s.uniq.nodup.tdf $Genomedir/${Genome}.chrom.sizes
    #wait
    #Statistic rRNA/tRNA/M reads for samples
    echo -en "$s.M\t" > $s.bg.sum
    samtools view $s.uniq.nodup.bam "M" |wc -l  >>$s.bg.sum
    loc="rRNA tRNA"
    for l in $loc
    do
        echo -en "$s.$l\t" >> $s.bg.sum
        intersectBed -abam $s.uniq.nodup.bam -b $Genomedir/$Genome.$l.bed |samtools view - |wc -l >> $s.bg.sum
        wait
    done
    # Caculate depth for samples
    bedtools genomecov -bga -pc -ibam $s.uniq.nodup.bam > $s.uniq.bg
    echo -en "track type=bedGraph name=$s color=200,100,0 altColor=0,100,200 priority=20\n">$s.bedGraph
    cat $s.uniq.bg |sort -k1,1 -k2,2n |awk '{print "chr"$1"\t"$2"\t"$3"\t"$4}'>> $s.bedGraph
    #bedGraphToBigWig $s.bedGraph $Genomedir$Genome.genome $s.bw
    wait
    bedtools intersect -a $s.uniq.bg -b $Genomedir/hg38.blacklist.bed -v -wa|sort -k1,1 -k2,2n> $s.blacklist.uniq.bed
    wait
    cat $s.blacklist.uniq.bed|awk -F "\t" '$4!=0{print $0}'|cut -f1-3 > $s.blacklist.uniq.nonzero.bed
    wait
    bedtools coverage -a window${split}.bed -b $s.blacklist.uniq.nonzero.bed> $s.blacklist.uniq.window${split}.bed
    wait
    A=$(tail -n1 $s.uniq.bg.cov.out|awk -F "\t" '$1=="all"{printf "%.2f%%",$2*100}')
    wait
    echo -en "$s\t$A\n" >> Summary.cov
    samtools view -H $s.uniq.nodup.bam | sed -e 's/SN:\([0-9XY]\)/SN:chr\1/' -e 's/SN:MT/SN:chrM/' | samtools reheader - $s.uniq.nodup.bam > $s.uniq.nodup.chr.bam
    samtools index $s.uniq.nodup.chr.bam
    nohup bamCoverage -b $s.uniq.nodup.chr.bam -bs 100000 -o $s.uniq.nodup.100000.bw -p 10 &
    nohup bamCoverage -b $s.uniq.nodup.chr.bam -bs 1000000 -o $s.uniq.nodup.1000000.bw -p 10 &
    nohup bamCoverage -b $s.uniq.nodup.chr.bam -bl $Genomedir/hg38.blacklist.bed -bs 500000 -o $s.uniq.nodup.500000rmbl.bw -p 10 &
    nohup bamCoverage -b $s.uniq.nodup.chr.bam -bl $Genomedir/hg38.blacklist.bed -bs 1000000 -o $s.uniq.nodup.1000000rmbl.bw -p 10 &
    inter=$(bedtools intersect -abam $s.uniq.nodup.bam -b $Genomedir/$Genome.intergenic.bed -f 1 -wa -bed|wc -l);
    intro=$(bedtools intersect -abam $s.uniq.nodup.bam -b $Genomedir/$Genome.intron.bed -f 1 -wa -bed|wc -l);
    exon=$(bedtools intersect -abam $s.uniq.nodup.bam -b $Genomedir/$Genome.exon.bed -f 1 -wa -bed|wc -l);
    centro=$(bedtools intersect -abam $s.uniq.nodup.bam -b $Genomedir/Centromere.bed -f 1 -wa -bed|wc -l);
    echo -en "$s\n$inter\n$intro\n$exon\n$centro\n" > $s.log.temp
done
