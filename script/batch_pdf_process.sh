#!/bin/bash
# batch loading 

echo "input directory: $1"
echo "output directory: $2"
IFS=$'\n';

#downloads the files from origin 

mkdir -p "$2/downloaded"


currpath=$(pwd)
cd "$2"
real2=`pwd -P`
cd "$currpath"
cd "$3"
real3=`pwd -P`

echo $real2 
echo $real3 
cd "$currpath"
#read -p "Paused real2 ..."
wget -r -l 0 -np "$1" -P "$2/downloaded/"

cd "$currpath"

for name in `find $real2/downloaded/ -name \*.pdf -print` # `ls -a $2/downloaded/*.pdf`  
do  
	newname="$(echo $name | sed 's/ /_/g')"
	mv "$name" $newname
	subdirname="$(echo $newname | rev | cut -d '.' -f2-10 | cut -d '/' -f1 | rev)"

	mkdir -p "$real2/data/$subdirname"

	echo $subdirname
	#copies the pdf there too
	cp $newname "$real2/data/$subdirname/"

	#runs pstotext 
	pstotext $newname > "$real2/data/$subdirname/""$subdirname"".pdf.xml"

	#runs metatagger

	cd "$real3"
	echo "$real2/data/$subdirname/""$subdirname"".pdf.xml -> $real2/data/$subdirname/""$subdirname"".pdf_runcrf.xml" | "$real3/bin/runcrf"

	cd "$currpath"
#	ls
#	pwd
#	read -p "Paused after ls and pwd ..."
	#runs imagemagick 
	convert -density 400 -verbose "$real2/data/$subdirname/""$subdirname"".pdf" "$real2/data/$subdirname/""$subdirname"".pdf.jpg"
	
	resident=`identify $real2/data/$subdirname/"$subdirname".pdf`

	result="$(echo $resident | cut -d ' ' -f3)"

	width="$(echo $result | cut -d 'x' -f1)"

	height="$(echo $result | cut -d 'x' -f2)"


	#creates the properties file 
	touch "$real2/""$subdirname"".properties"

	echo "ispdfalreadyparsed=yes" >> "$real2/""$subdirname"".properties"
	echo "height=$height" >> "$real2/""$subdirname"".properties"
	echo "width=$width" >> "$real2/""$subdirname"".properties"
	echo "pdflocation=$real2data/$subdirname/""$subdirname"".pdf" >> "$real2/""$subdirname"".properties"
	echo "imagedir=$real2data/$subdirname/" >> "$real2/""$subdirname"".properties"


#    ispdfalreadyparsed=true
#    height=792
#    width=600
#    pdflocation=/pstotext/data/741608/741608.pdf
#    imagedir=/pstotext/data/741608

	echo $result
	echo "width: $width ; height: $height" 

done



IFS=$' \t\n'