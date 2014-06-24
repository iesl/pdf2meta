#!/bin/bash
# batch loading 

echo "input directory: $1"
echo "output directory: $2"
IFS=$'\n';

#downloads the files from origin 

mkdir -p "$2/downloaded"

wget -r -l 0 -np "$1" -P "$2/downloaded/"
for name in `find $2/downloaded/ -name \*.pdf -print` # `ls -a $2/downloaded/*.pdf`  
do  
	newname="$(echo $name | sed 's/ /_/g')"
	mv "$name" $newname
	subdirname="$(echo $newname | rev | cut -d '.' -f2-10 | cut -d '/' -f1 | rev)"

	mkdir -p "$2/data/$subdirname"

	echo $subdirname
	#copies the pdf there too
	cp $newname "$2/data/$subdirname/"

	#runs pstotext 
	pstotext $newname > "$2/data/$subdirname/""$subdirname"".pdf.xml"

	#runs metatagger
	cd "$3"
	echo "$2/data/$subdirname/""$subdirname"".pdf.xml -> $2/data/$subdirname/""$subdirname"".pdf_runcrf.xml" | "$3/bin/runcrf"

	#runs imagemagick 
	convert -density 400 -verbose "$2data/$subdirname/""$subdirname"".pdf" "$2data/$subdirname/""$subdirname"".pdf.jpg"
	
	resident=`identify $2data/$subdirname/"$subdirname".pdf`

	result="$(echo $resident | cut -d ' ' -f3)"

	width="$(echo $result | cut -d 'x' -f1)"

	height="$(echo $result | cut -d 'x' -f2)"


	#creates the properties file 
	touch "$2/""$subdirname"".properties"

	echo "ispdfalreadyparsed=yes" >> "$2/""$subdirname"".properties"
	echo "height=$height" >> "$2/""$subdirname"".properties"
	echo "width=$width" >> "$2/""$subdirname"".properties"
	echo "pdflocation=$2data/$subdirname/""$subdirname"".pdf" >> "$2/""$subdirname"".properties"
	echo "imagedir=$2data/$subdirname/" >> "$2/""$subdirname"".properties"


#    ispdfalreadyparsed=true
#    height=792
#    width=600
#    pdflocation=/pstotext/data/741608/741608.pdf
#    imagedir=/pstotext/data/741608

	echo $result
	echo "width: $width ; height: $height" 

done



IFS=$' \t\n'