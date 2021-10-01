#!/bin/bash
# -*- coding: utf-8 -*-

##get the present directory
WD=$(pwd)

#define output files
INFILE1="algn_Myanmar_fisk209_01.fasta"

new_edited_fasta_files=$INFILE1
# initialize dna sequence with empty string
dna_seq=''

# function to write out a sequence file
function write_dna_sequence_file {
  # give arguments passed into function more descriptive names
  file_name=$1
  species_name=$2
  dna_seq=$3
  echo ">${species_name}" >> ${file_name}
  echo "${dna_seq}" >> ${file_name}
}

# loop over all fas files in the current directory
for file in $new_edited_fasta_files
do
  # read individual fas file line by line
  while read line
  do
    if [[ $line == ">"* ]] # if line starts with '>'
    then
      # getting the 1st, 2nd and 4th element and doing minor modifications
      #species_name=$(echo ${tmp} | cut -d";" -f1,2 | sed 's/fasta//g')
      # Write out dna sequence for species and gene
      # Note: If we are at the very first line of a fas file: don't write because we first have to read the dna sequence
      if [ "${dna_seq}" != '' ]
		 then
        write_dna_sequence_file "${file_name}" "${species_name}" "${dna_seq}"
  		#echo $file_name
		fi
      # remove leading > from line just read and assign it to variable tmp
      tmp=$(echo ${line:1})
	  #echo $tmp
      # extract species name, gene name and range from tmp variable by splitting tmp at each '_',
      # getting the 1st, 2nd and 4th element and doing minor modifications
      species_name=$(echo ${tmp} | cut -d"_" -f1,2 | sed 's/fasta//g')
      #echo $species_name
	  #gene_name=$(echo ${tmp} | cut -d";" -f4 | sed 's/ //g' | sed 's/(/_/g' | sed 's/)//g')
      #range=$(echo ${tmp} | cut -d";" -f2 | sed 's/ //g')
      # generate file name
	  file_name=$(echo $file | sed -E 's/_[0-9][0-9].fasta/_02.fasta/g')
	  #echo $file_name
	  #file_name="${species_name}.${gene_name}.${range}.${postfix}"
      # set dna_seq variable back to an empty string
      dna_seq=''
    else # if line doesn't start with >, append line to dna sequence
      dna_seq="${dna_seq}${line}"
    fi
  done < ${file}
  # we reached the end of a fas file here. write out last sequence file
  # and set dna_seq to an empty string
  write_dna_sequence_file "${file_name}" "${species_name}" "${dna_seq}"
  dna_seq=''
done
#
OUTFILE1=$file_name
echo "$OUTFILE1"
grep '>' "$OUTFILE1"