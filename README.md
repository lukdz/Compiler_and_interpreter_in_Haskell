# Kompilator i interpreter w Haskellu

Program powstał w ramach pracowni z Metod Programowania (w semestrze letni 2016/2017) na wydziale Informatyki Uniwersytetu Wrocławskiego.


## Wersje

Repozytorium zawiera 3 wersje będące kolejnymi iteracjami zadania, z których każda kolejna wspiera poszerzony zbiór prostego językiem HDML wymyślonego na potrzeby tego zadania

| Kompilator | Interpreter | Wspierane elementy języka |
| --- | --- | --- |
|  | `inter4` | Wyrażenia arytmetyczno-logiczne |
| `comp5` | `inter5` | + funkcje rekurencyjne (pierwszego rzędu), pary oraz listy |
| `comp6` | `inter6` | + funkcje wyższego rzędu oraz funkcje anonimowe |


## Kod źródłowy

| Zadanie | Lokalizacja pliku |
| --- | --- |
| `inter4` | `interpreters/inter4/LukaszDzwoniarek.hs` |
| `inter6` | `interpreters/inter6/LukaszDzwoniarek.hs` |
| `inter5` | `interpreters/inter5/LukaszDzwoniarek.hs` |
| `comp5` | `compilers/comp5/LukaszDzwoniarekCompiler.hs` |
| `comp6` | `compilers/comp6/LukaszDzwoniarekCompiler.hs` |


### Wymagania

W celu uruchomienia oprogramowanie niezbędny jest:

```
Glasgow Haskell Compiler = GHC
kompilator C
```


## Kompilator


### Kompilacja

Kompilacja kompilatora (dla wersji 6):

```
# cd compilers/comp5/
ghc Comp5
# cd ../..
```

<a name="emu6809">
Kompilacja emulatora procesora Motorola 6809

```
# cd compilers/emu6809
make
cp emu6809 ../comp5/
cp emu6809 ../comp6/
# cd ../..
```
</a>


### Testowanie poszczególnych programów

Kompilacja do pliku:

```
# cd compilers/comp5
./Comp5 ./JST/sum.pp5
```

Kompilacja na ekran (wyświetlenie "kodu wynikowego" w postaci instrukcji):

```
# cd compilers/comp5
./Comp5 -m ./JST/sum.pp5
```

Wykonanie skompilowanego programu

```
# cd compilers/comp5
./emu6809 ./JST/sum.b09
```


### Zbiorcze wykonanie testów

Skrypt, który porównuje wyniki działania zbioru programów testowych wykonanych na interpreterze i kompilatorze+emulatorze:

1. Otwórz katalog (comp5 lub comp6)

```
cd compilers/comp6
```

2. Nadaj uprawnienia skryptowi wykonującemu testy

```
chmod +x test.sh
```

3. Umieść w katalogu (comp5 lub comp6) wykonywalne pliki:
   - [emu6809](#emu6809)
   - Prac5 lub [Prac6](#Prac6)

4. Uruchom testy

```
./test.sh
```

Wykonanie wszystkich testów powinno zająć ok. 5 sekund.

Wyniki mojego kompilatora znajdują się w pliku testReslut. Nie ręczę za nie, jeśli uzyskasz inną odpowiedź porównaj z wynikiem, uzyskiwanym przez Prac6 na tym pliku. Jeśli sądzisz, że błąd jest w moich odpowiedziach proszę daj mi znać.


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

* mgr Piotr Polesiuk (https://github.com/poles-p) - autor pracowni z MP (sprawdzaczki, emulator)
* Adam Sawicki (https://github.com/Asdamos) - udostępnione zestawy testów
* Paweł Smolak (https://github.com/psmolak) - zebrane i udostępnione zestawy testów studentów przedmiotu
* uczestnicy pracowni z Metod Programowania w roku 2016/2017, którzy udostępnili swoje zestawy testów
