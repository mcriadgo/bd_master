# -*- coding: utf-8 -*-
import sys
import deaths_map_reduce as mr
import pandas as pd
import argparse

def create_mr_file(year, nombre_archivo):
    """
    Genera un  fichero .csv en la misma carpeta donde se sitúe este script
    El script tendrá los datos de un año pasado como argumento y cada linea del 
    fichero generado tendrá un separador de campos '#'
    Parameters
    ----------
    year : INT
        Año por el que filtrar la tabla de datos
    nombre_archivo : STR
        Nombre del fichero generado

    Returns
    -------
    None.

    """
	
    df = pd.read_csv('./annual-number-of-deaths-by-cause.csv')
    filtered_df = df[df['Year']==year]
    filtered_df.to_csv(nombre_archivo, index=False, header=None, sep='#')
    

def mr_job(nombre_archivo, annio):
    assert 0<=annio<2018, 'El año debe estar entre 0 y 2017'
    assert nombre_archivo.endswith('csv'), 'El fichero debe tener extensión csv'
    create_mr_file(annio, nombre_archivo)
    mr_job = mr.MRDeathsByCause(args=[nombre_archivo])
    results = []
    #se ejecuta el .py con el código map-reduce generado en el apartado anterior
    with mr_job.make_runner() as runner:
        runner.run()
        for key, value in mr_job.parse_output(runner.cat_output()):
            results.append((key, value))
    return results


if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('file', type=str, help='Fichero para guardar annual-number-of-deaths-by-cause.csv filtrado e insertalo en mrjob. Debe llevar extension csv')
    parser.add_argument('year', type=int, help='Año por el que filtrar el fichero')
    args = parser.parse_args()
    nombre_archivo = args.file
    annio = int(args.year)
    print(mr_job(nombre_archivo, annio))


