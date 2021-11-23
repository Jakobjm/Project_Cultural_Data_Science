# -*- coding: utf-8 -*-

output = open('outputs/clean_BBR_cph_data.json', 'a')


with open('data/bbr_cph_byg.json', buffering = 200000)as f:
    for line in f:
        if line.startswith("}"):
            output.write(line)
        elif line.startswith(",{"):
            output.write(line)
        elif line.startswith("{"):
            output.write(line)
        elif line.startswith("]"):
            output.write(line)
        elif line.startswith('"BygningList"'):
            output.write(line)
        elif line.startswith('"byg026'):
            output.write(line)
        elif line.startswith('"byg054AntalEtager"'):
            output.write(line)
        elif line.startswith('"byg404Koordinat"'):
            output.write(line)
        elif line.startswith('"byg406Koordinatsystem"'):
            truncLine = line[:-2]
            output.write(truncLine)
        elif line.startswith('"byg021BygningensAnvendelse"'):
            output.write(line)
        elif line.startswith('"id_lokalId"'):
            output.write(line)
        else: 
            saveline = line.replace(line,"")
            output.write(saveline)
            
            