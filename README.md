# Postavljanje Projekta

Prije pokretanja projekta potrebno je instalirati Common Lisp. Možete pratiti sljedeći video vodič za instalaciju:

[![Instalacija Lisp-a](https://img.youtube.com/vi/T61IN5FHr8g/0.jpg)](https://www.youtube.com/embed/T61IN5FHr8g?si=NBiZ0Xn46Cwk6FNE)

## Instalacija QuickLisp-a

Također, potrebno je instalirati **QuickLisp**, sustav za upravljanje paketima za programski jezik Common Lisp. QuickLisp omogućuje jednostavno dohvaćanje potrebnih biblioteka za rad s mrežnim socketima i JSON datotekama.

## Instalacija Tkinter-a

Za serversku stranu, potrebno je instalirati **Tkinter** biblioteku. To možete učiniti s jednom od sljedećih naredbi, ovisno o vašem sustavu:

- Na Ubuntu/Linux sustavu:
  ```bash
  sudo apt-get install python3-tk
  ```

- Na ostalim sustavima koristeći `pip`:
  ```bash
  pip install tk
  ```

## Pokretanje Projekta

1. Pokrenite Python server:
   ```bash
   python server.py
   ```

2. Pokrenite aplikaciju pisanju u Lisp-u:
   ```bash
   sbcl --script .\app.lisp
   
