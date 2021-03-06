# Kompilator i interpreter w Haskellu

Projekt został zrealizowany w ramach pracowni przedmiotu Metody Programowania (w semestrze letni 2016/2017) na wydziale Informatyki Uniwersytetu Wrocławskiego.

W ramach zadania napisałem kompilator i interpreter działący z językiem programowania HDML wg. dostarczonej specyfikacji. Kod wykonywalny jest przeznaczony dla procesora Motorola 6809, którego emulator został stworzony przez prowadzącego pracownię ([poles-p](#poles-p)) na podstawie dokumentacji technicznej.


## Wersje

Repozytorium zawiera 3 wersje będące kolejnymi iteracjami zadania, z których każda kolejna wspiera poszerzony zbiór instrukcji prostego języku HDML stworzonego na potrzeby pracowni.

| Kompilator | Interpreter | Wspierane elementy języka |
| --- | --- | --- |
|  | `inter4` | Wyrażenia arytmetyczno-logiczne |
| `comp5` | `inter5` | + funkcje rekurencyjne (pierwszego rzędu), pary oraz listy |
| `comp6` | `inter6` | + funkcje wyższego rzędu oraz funkcje anonimowe |


## Kod źródłowy

| Zadanie | Lokalizacja pliku z kodem źródłowym |
| --- | --- |
| `inter4` | `interpreters/inter4/LukaszDzwoniarek.hs` |
| `inter6` | `interpreters/inter6/LukaszDzwoniarek.hs` |
| `inter5` | `interpreters/inter5/LukaszDzwoniarek.hs` |
| `comp5` | `compilers/comp5/LukaszDzwoniarekCompiler.hs` |
| `comp6` | `compilers/comp6/LukaszDzwoniarekCompiler.hs` |


### Wymagania

W celu uruchomienia oprogramowanie niezbędny są:

```
Glasgow Haskell Compiler = GHC
Kompilator C
```


## Kompilator


### Kompilacja

Kompilacja kompilatora (w wersji 6):

```
# cd compilers/comp6/
ghc Comp6
# cd ../..
```

<a name="emu6809">
Kompilacja emulatora procesora Motorola 6809:

```
# cd compilers/emu6809
make
cp emu6809 ../comp5/
cp emu6809 ../comp6/
# cd ../..
```
</a>


### Testowanie poszczególnych programów

Plik `./JST/fib.pp5` zawiera przykładowy program obliczający n-tą liczbę Ciągu Fibonacciego.

Kompilacja kodu źródłowego do pliku wykonywalnego (na procesorze Motorola 6809 lub emulatorze [emu6809](#emu6809)):

```
# cd compilers/comp6
./Comp6 ./JST/fib.pp5
```

Kompilacja na "ekran" (wyświetlenie kodu wynikowego w postaci instrukcji):

```
# cd compilers/comp6
./Comp6 -m ./JST/fib.pp5
```

Wykonanie skompilowanego programu przy pomocy emulatora:

```
# cd compilers/comp6
./emu6809 ./JST/fib.b09
```


### Testowanie zbioru programów

Skrypt, który porównuje wyniki, działania przykładowych programów, uzyskanych na interpreterze, z otrzymanymi przy pomocy kompilatora i emulatora:

1. Otwórz katalog comp6:

   ```
   cd compilers/comp6
   ```

2. Nadaj uprawnienia skryptowi wykonującemu testy:

   ```
   chmod +x test.sh
   ```

3. Umieść w katalogu comp6 wykonywalne pliki:

   - [emu6809](#emu6809)
   ```
   cp ../emu6809/emu6809 ./
   ```

   - [Prac6](#Prac6)
   ```
   cp ../../interpreters/inter6/Prac6 ./
   ```

4. Uruchom testy

   ```
   ./test.sh
   ```

Wykonanie wszystkich testów powinno trwać ok. 5 sekund.

Wyniki mojego kompilatora znajdują się w pliku testReslut


## Testowanie interpretera

<a name="Prac6">
Kompilacja interpretera, oraz sprawdzaczki

```
# cd interpreters/inter6
ghc Prac6
```
</a>

Wykonanie wszystkich testów

```
# cd interpreters/inter6
./Prac6 -t
```


## Zbudowanie z wykorzystaniem

* [GHC](https://www.haskell.org/ghc/) - The Glasgow Haskell Team


## Podziękowania

* <a name="poles-p">mgr Piotr Polesiuk (https://github.com/poles-p) - autor pracowni z MP (sprawdzaczki, emulator)</a>
* Adam Sawicki (https://github.com/Asdamos) - udostępnione zestawy testów (6AST)
* Jan Sierpina - udostępnione zestawy testów (JST)
* Paweł Smolak (https://github.com/psmolak) - zebrane i udostępnione zestawy testów studentów przedmiotu
* uczestnicy pracowni z Metod Programowania w roku 2016/2017, którzy udostępnili swoje zestawy testów
