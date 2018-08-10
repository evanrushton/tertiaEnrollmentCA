#!bin/bash
for file in *.png
do
  mv "$file" ${file/dlfile.aspx?cLevel=School&cYear=/} 
done
i for file in *.asp
do mv "$file" ${file/&cCat=Enrollment&cPage=filesenr.asp/.txt}
done
