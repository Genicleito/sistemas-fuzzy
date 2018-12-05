cd ../../Datasets/
mkdir partes-enem
sh ../Trabalho/Scripts/cortarArquivo.sh
# split -l 250000 MICRODADOS_ENEM_2017.csv partes-enem
cd partes-enem
Rscript ../../Trabalho/Codigos/unir-partes-dataset.r