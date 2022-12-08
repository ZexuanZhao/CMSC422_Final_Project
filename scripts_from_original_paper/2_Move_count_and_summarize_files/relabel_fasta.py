#! /usr/bin/env python

from Bio import SeqIO

file = "cytb_copy.afa"
newfile = "cytb_short.afa"

SeqList = []

with open(file, "r+") as f:
	records = SeqIO.parse(file, 'fasta')
	for record in records:
		print record.id
		l = record.id.split('_', 1)[0]
		print l
		record.id = l
		print record.id
		record.description = l
		SeqList.append(record, newfile, 'fasta')
		