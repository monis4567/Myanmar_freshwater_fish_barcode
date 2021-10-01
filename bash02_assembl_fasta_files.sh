#!/bin/bash
# -*- coding: utf-8 -*-

#put present working directory in a variable
WD=$(pwd)

OUTF01="bilag01_all_freshwater_fish_from_Myanmar_seq_01.fasta"
rm "${WD}"/"${OUTF01}"
#write a new output file to write to
touch "${WD}"/"${OUTF01}"
iconv -f UTF-8 -t UTF-8 "${WD}"/"${OUTF01}"
#iterate over all fasta files
for f in algn*fasta
do 
	cat "${f}" >> "${WD}"/"${OUTF01}"
done