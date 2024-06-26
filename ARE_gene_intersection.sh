module load gcc/13.3.0 bedtools/2.30.0 samtools/1.17

samtools faidx genome.fa
cut -f 1,2 genome.fa.fai > chrom.sizes #this gives chromosome sizes

#add flank to genes

flankBed -i Genes_pos.bed -g chrom.sizes -b 200000 > flank_scun_genes_pos.bed #200000 gives 100000 each way

intersectBed -wa -wb -a Sceloporus\ AREs_PWMTools_11scaffolds_PosStrand.bed -b flank_scel_genes_pos.bed > und_genes_and_AREs_pos.bed
