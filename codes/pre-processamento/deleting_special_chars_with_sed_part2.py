import os

seds = [
    "sed -r 's/\\;//g' /tmp/teste.csv > " + target_aux + " # apaga todos os ; presentes na base",
    "sed -r 's/\\\"\,\\\"/;/g' " + target_aux + " > " + target + " # substitui o separador por ;",
    
    # Adicionada a expresão '+'' aos seds que consideram a presença de contra-barra para permitir afetar uma ou mais contra-barras
    "sed -r 's/\\\\+\;/;/g' " + target + " > " + target_aux + " # remove escapes de separador",
    "sed -r 's/\\\\+\\\"//g' " + target_aux + " > " + target + " # remove aspas escapadas",

    "sed -r 's/^\\\"|\"$/_cidacs_control_quotes_/g' " + target + " > " + target_aux + " # guarda as aspas dos campos",
    "sed -r 's/\\\"|\,//g' " + target_aux + " > " + target + " # remove aspas não escapadas e vírgulas no meio dos campos",
    
    "sed -r 's/\\;/\\\"\\,\\\"/g' " + target + " > " + target_aux + " # volta o separador para , e resgata as aspas do final de cada campo",
    "sed -r 's/_cidacs_control_quotes_/\\\"/g' " + target_aux + " > " + target + " # resgata as aspas do início e final das linhas"
]
for sed in seds:
    error = False
    print("Appling: " + sed)
    if(os.system(sed) != 0):
        error = True
        print("[ERROR]")
        break
if not error:
    print("SUCESS")

# Function to handle CSV files by removing characters such as quotation marks, backslash, comma, semicolon.
def convertCSV(source, target, sep):
    import os
    if sep == ",":
        sep_aux = ";"
    elif sep == ";":
        sep_aux = ","

    target_aux = target + ".part"
    seds = [
        "sed -r 's/\\" + sep_aux + "//g' " + source + " > " + target_aux,           # remove todas as ocorrecias do separador auxiliar na base
        "sed -r 's/\\\"\\" + sep + "\\\"/" + sep_aux + "/g' " + target_aux + " > " + target,      # substitui o separador pelo separador auxiliar",
        
        # Adicionada a expresão '+'' aos seds que consideram a presença de contra-barra para permitir afetar uma ou mais contra-barras
        "sed -r 's/\\\\+\\" + sep_aux + "/" + sep_aux + "/g' " + target + " > " + target_aux,     # remove contra-barras presentes antes do separador auxiliar",
        "sed -r 's/\\\\+\\\"//g' " + target_aux + " > " + target,                   # remove aspas escapadas com contra-barra",

        "sed -r 's/^\\\"|\"$/_cidacs_control_quotes_/g' " + target + " > " + target_aux,    # guarda as aspas que delimitam os campos",
        "sed -r 's/\\\"|\\" + sep + "//g' " + target_aux + " > " + target,                  # remove aspas não escapadas e caracteres iguais ao separador no meio dos campos",
        
        "sed -r 's/\\" + sep_aux + "/\\\"\\,\\\"/g' " + target + " > " + target_aux,    # define o separador como vírgula e restaura as aspas delimitadoras dos campos",
        "sed -r 's/_cidacs_control_quotes_/\\\"/g' " + target_aux + " > " + target      # restaura as aspas no início e fim das linhas"
    ]

    for sed in seds:
        print("Appling: " + sed)
        if(os.system(sed) != 0):
            print("==> [ERROR]")
            os.system("rm -rf " + target + "*")
            return
    print("SUCESS")
    os.system("rm -rf " + target_aux)