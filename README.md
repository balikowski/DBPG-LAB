# DBPG-LAB

Aplikacja webowa zbudowana w środowisku **R** z wykorzystaniem frameworku **Shiny**, służąca do interaktywnej analizy danych, uczenia maszynowego oraz klasteryzacji. Projekt został stworzony z myślą o prostym i szybkim testowaniu różnych modeli predykcyjnych bezpośrednio z poziomu przeglądarki.

## Funkcje aplikacji

- **Zarządzanie danymi:**
  - Wbudowane zbiory danych: `iris`, `Boston` oraz `mtcars`.
  - Możliwość importu własnych plików CSV.
  - Automatyczny wstępny preprocessing danych.
  - Podgląd tabelaryczny oraz podstawowe statystyki opisowe załadowanego zbioru.

- **Regresja:**
  - Obsługa modeli takich jak: Regresja Liniowa, Random Forest oraz SVR.
  - Interaktywny wybór zmiennej objaśnianej ($Y$) oraz objaśniających ($X$).
  - Podgląd wyników (podsumowanie modelu) oraz generowanie wykresów diagnostycznych.

- **Klasyfikacja:**
  - Obsługa modeli: Regresja Logistyczna, SVM (Support Vector Machines) oraz Random Forest.
  - Automatyczne kodowanie zmiennych kategorycznych (*Label Encoding* / inżynieria cech tekstowych).
  - Zakładka dedykowana **predykcji dla nowej obserwacji** na podstawie ręcznie wprowadzonych parametrów.

- **Klasteryzacja (Analiza skupień):**
  - Obsługa algorytmów takich jak klasteryzacja hierarchiczna (`hclust` / AGNES).
  - Wizualizacja wyników w formie wykresów diagnostycznych i dendrogramów (np. z użyciem biblioteki `ggplot2` oraz `patchwork`).

## Struktura projektu

```text
DBPG-LAB/
├── app.R               # Główny punkt wejścia aplikacji Shiny
├── ui.R                # Definicja interfejsu użytkownika (Layout stron)
├── server.R            # Logika rekatywna serwera aplikacji
└── R/                  # Moduły i funkcje pomocnicze
    ├── preprocess.R    # Czyszczenie danych i obsługa brakujących wartości
    ├── model_spec.R    # Budowanie specyfikacji modeli i enkodowanie zmiennych
    ├── ui_components.R # Komponenty UI wielokrotnego użytku
    └── server_helpers.R# Funkcje pomocnicze dla logiki serwera i predykcji
