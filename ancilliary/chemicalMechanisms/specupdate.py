#!/usr/bin/env python

import sqlite3
import xlrd

conn=sqlite3.connect("../SPECIATE 4.3 9-28-2011-FINAL.sqlite.new")
wb = xlrd.open_workbook('chemicals.xls')
sh = wb.sheet_by_index(0)
c = conn.cursor()

for rownum in range(sh.nrows):
	if rownum != 0:
		name = sh.row_values(rownum)[0]
		group = sh.row_values(rownum)[2]
		if group not in ["",0.]:
			cmd = "UPDATE SPECIES_PROPERTIES set RADM2_GROUP=\"%s\" WHERE NAME=\"%s\""%(group,name)
			#print cmd
			c.execute(cmd)

conn.commit()
conn.close()
