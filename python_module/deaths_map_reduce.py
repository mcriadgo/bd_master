# -*- coding: utf-8 -*-

from mrjob.job import MRJob

class MRDeathsByCause(MRJob):
    """
    """

    def mapper(self, _, line):
        """
        Lee cada linea del fichero, separa la linea por el delimitador '#'.
        Genera un diccionario cuya clave será el nombre de la causa y cuyo 
        valor será el numero de muertes para esa causa.
        Parameters
        -----------
        line: STR
            linea del fichero
        
        Yields
        --------
        DICTIONARY {STR: FLOAT}
            diccionario con clave nombre de causa y con valor numero de muertes por 
            esa causa
        
        """
        causas = ["Number of executions (Amnesty International)","Road injuries","Cirrhosis and other chronic liver diseases",
                  "Digestive diseases","Tuberculosis","HIV/AIDS","Diarrheal diseases","Intestinal infectious diseases",
                  "Lower respiratory infections","Meningitis","Drowning","Alzheimer disease and other dementias",
                  "Parkinson disease","Alcohol use disorders","Drug use disorders","Malaria","Maternal disorders",
                  "Neonatal disorders","Nutritional deficiencies","Diabetes mellitus","Chronic kidney disease",
                  "Chronic respiratory diseases","Conflict and terrorism","Hepatitis","Neoplasms",
                  "Fire, heat, and hot substances","Poisonings","Exposure to forces of nature","Environmental heat and cold exposure",
                  "Protein-energy malnutrition","Cardiovascular diseases","Self-harm","Interpersonal violence","Terrorism (deaths)"]
        
        splitted_line = line.replace('\n','').split('#')
        for i, element in enumerate(splitted_line[3:]):
            yield causas[i], float(element.replace('>', '')) if element != '' else 0

    def reducer(self, key, values):
        """
        Reduce por clave de diccionario. Recoge todos los elementos del diccionario con la misma
        clave y suma sus valores
        
        Parameters
        ----------
        key : STR
            Nombre de la causa
        values : FLOAT
            Numero de muertes para esa causa para un país determinado

        Yields
        ------
        DICTIONARY {STR: FLOAT}
            diccionario cuyas claves son las causas de mortalidad y cuyo valor
            es la suma de las muertes por esa causa para todos los países

        """
        yield key, sum(values)
        
if __name__ == '__main__':
    MRDeathsByCause.run()