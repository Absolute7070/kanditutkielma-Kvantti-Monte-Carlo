# Johdanto

Tämä on osa kanditutkielmaa, jossa käytän variaatio-Monte Carlo -menetelmää atomien perustilan energian ja aaltofunktion ratkaisemiseksi. Tutkielman tarkoituksena on luoda helposti ymmärrettävän algoritmin, joka vaatii vain vähän taustatietoa sen ymmärtämiseksi, ja toteuttaa algoritmi jollain ohjelmointikielellä. Tässä koko paketissa käytin Fortran, Python ja Mathematica. Python-ohjelmalla yhdistän Mathematica ja Fortran niinikään yhteen. 

- **Tärkeimmät tiedostot:**
    - Mathematica:
        - .nb-päätteiset tiedostot: tulostaa atomin paikallisen energian ja aaltofunktion lausekkeet.
    - Python:
        1. converter.ipynb: kääntää Mathematica tulostefunktiot Fortran-kelpoiseksi. 
    - Fortran:
        1. optimization.f90: optimoi parametrit
        2. main.f90: laskee energia 

# Vaatimukset

- Fortran:
    - uusin GNU-compiler
- Python
    - Jupyter notebook tai jupyter lab
- Mathematica notebook
    - omien atomien paikallisen energian ja aaltofunktion neliön muodostamiseksi. Valmiina on vain heliumatomin.

# ennen käyttöä

- **optimization.f90:** optimoidaan parametrejä.
    - Aseta alkuarvot kommenttiosaan
        
        ```fortran
        !!!!!!!WHEN CHANGE ATOM, CHANGE THESES!!!!!!!!!!!
        
        ...
        
        !!!!!!!END OF: WHEN CHANGE ATOM, CHANGE THESES!!!!!!!!!!!
        ```
        
    - Tarvittaessa voit vaihtaa gradient descent -menetelmässä käytettyä finite difference metodia subrutiinissa `gradient_atapoint`.
    - Finite difference subrutiinin askeleen suuruutta säädetään vastaavissa subrutiinissa säätämällä `h`-arvoa: `fivePointDiff` -(viiden pisteen differenssimenetelmä) tai `twoPointDiff` (kahden pisteen differenssimenetelmä)

- **main.f90**: lasketaan energiaa edellä saatujen optimoitujen parametrien avulla
    - Aseta alkuarvot kommenttiosaan: käytännössä alkuarvot ovat samat kuin **optimization.f90**-tiedostossa, mutta muuta `numberOfParallel` suuremmaksi (esim. 8), jotta saadaan mahdollisimman paljon rinnakkaisia elektronikonfiguraatioavaruuksia (ja siten riippumattomia energian arvioita).
        
        ```fortran
        !!!!!!!WHEN CHANGE ATOM, CHANGE THESES!!!!!!!!!!!
        
        ...
        
        !!!!!!!END OF: WHEN CHANGE ATOM, CHANGE THESES!!!!!!!!!!!
        ```
        

# Käyttö

## Mathematica tiedosto (valinnainen)

- Jos haluat rakentaa omia atomia, käy tämän osion läpi.

1. Mene .nb päätteisiin tiedostoihin ja muokkaa alkuosaa, jossa on **********input**********-sanat. 
2. Sen jälkeen mene tiedoston loppuun **************Save to the file************** -osioon ja muokkaa tiedoston polut, mihin tulostefunktiot laitetaan. 

## Python tiedosto (valinnainen)

1. Mene `python_converter/converter.ipynb` ja aseta polut: Mathematican tulosteiden polut ja Python tulosteeen polut. 
2. Siirrä Python tulosteet Fortran tiedostoon. 

## Fortran tiedosto

1. Moduulien kompilaatio: 
    
    ```python
    gfortran -O3 -c parameters.f90 fparser.f90 mersennemod.f90
    ```
    
2. optimization.f90: optimoi parametrit 
    
    ```kotlin
    gfortran -O3 -c optimization.f90 -ffree-line-length-none -fopenmp
    gfortran optimization.o parameters.o fparser.o mersennemod.o -fopenmp
    ./a.out <probability function's filename> <localEnergy's filename>
    ```
    
3. main.f90: laskee energia 
    
    ```kotlin
    gfortran -O3 -c main.f90 -ffree-line-length-none -fopenmp
    gfortran main.o parameters.o fparser.o mersennemod.o -fopenmp
    ./a.out <probability function's filename> <localEnergy's filename>
    ```
    

- Valmiiksi säädettyjen koodien ajaaminen:
    - optimization.f90:
        
        ```kotlin
        
        gfortran -O3 -c optimization.f90  -ffree-line-length-none -fopenmp
        gfortran optimization.o parameters.o fparser.o mersennemod.o  -fopenmp
        ./a.out probability_Refine localEnergy_Refine
        ```
        
    - main.f90
        
        ```python
        gfortran -O3 -c main.f90  -ffree-line-length-none -fopenmp
        gfortran main.o parameters.o fparser.o mersennemod.o  -fopenmp
        ./a.out probability_Refine localEnergy_Refine
        ```
        

### Muut käytännölliset

Testissä käytetyt alkuarvot voi täyttää taulukkoon muistamiseksi: 

- `optimization.f90:`
    - Settings
        
        
        | Atomi | Helium |
        | --- | --- |
        | Parametrit |  |
        | Parallelleiden määrä | 4 |
        | Koordinaatti-alue (Hatree yksikkö, Bohrin säde) | [-3, 3] |
        | hiukkasten lkm | 2 |
        | konfiguraatioiden määrä  | 1000 |
        | iteraatioiden määrä termalisaatiossa | 1500 |
        | intervalli termalisaatiossa | 10 |
        | learning rate | 0.001 |
        | energian toleranssi | 0.00000000000000000001 |
        | maximi steppien määrä | 50 |
    - Results:
        
        
        | parameters |  |
        | --- | --- |
- `main.f90`
    - Settings:
        
        
        | Atomi | Helium |
        | --- | --- |
        | Parametrit |  |
        | Parallelleiden määrä | 8 |
        | Koordinaatti-alue (Hatree yksikkö, Bohrin säde) | [-3, 3] |
        | hiukkasten lkm | 2 |
        | konfiguraatioiden määrä  | 1000 |
        | iteraatioiden määrä termalisaatiossa | 1500 |
    - results:
        
        
        |  | Laskettu | kirjallisuusarvo |
        | --- | --- | --- |
        | Energia  |  | −2.90338583(13) |
        | Varianssi |  |  |
        | Keskiarvon keskivirhe |  |  |

# Edistäminen

Kaikki muokkaukset ovat tervetulleita. Jos kyseessä ovat suuret muutokset, avaa ensin kysymys ja keskustele siitä, mitä haluaisit muuttaa.

En välttämättä vastaa heti, koska on muita tekemistä aina. :)

# Lisenssi

MIT
