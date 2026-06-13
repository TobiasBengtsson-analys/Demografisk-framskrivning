# Demografisk framskrivning
Kohortbaserad demografisk framskrivning av Sverige 2024–2049

## Om projektet
Detta projekt analyserar Sveriges framtida befolkningsutveckling och försörjningsbörda under perioden 2024–2044. 

Analysen bygger på en demografisk kohortkomponentmodell där befolkningen framskrivs utifrån antaganden om fertilitet, dödlighet och nettoflyttning. Utöver ett basscenario genomförs även alternativa scenarier samt en sannolikhetsbaserad osäkerhetsanalys baserad på Monte Carlo-simulering.

Projektet är utvecklat i R och resultaten presenteras i en interaktiv Quarto-rapport.

## Rapport
Den färdiga rapporten finns publicerad via GitHub Pages:

**[Klicka här för att läsa rapporten live](https://tobiasbengtsson-analys.github.io/Demografisk-framskrivning/)**

### Rapporten innehåller:
* **Befolkningsframskrivning 2024–2044:** Analys av förändringar i den demografiska åldersstrukturen.
* **Scenarioanalys:** Jämförelser för fertilitet, dödlighet och migration.
* **Försörjningskvoter:** Framskrivning av ungdoms-, äldre- och total försörjningskvot.
* **Monte Carlo-simuleringar:** Fixerade resultat från 1 000 simulerade framtider för att belysa osäkerheten utan långa laddningstider.
* **Interaktiva visualiseringar:** Dynamiska diagram över befolkningsutvecklingen.

## Metod
### Kohortkomponentmodell
Framskrivningen bygger på en kohortkomponentmodell där befolkningen åldras ett år i taget. För varje tidssteg beaktas:
* Födelser
* Dödsfall
* Nettoflyttning

Modellen utgår från Sveriges faktiska befolkning år 2024 uppdelad efter ålder och kön.

### Scenarioanalys
För att analysera känsligheten i resultaten jämförs ett basscenario med tre alternativa utvecklingsbanor:
* **Hög migration:** Ökat migrationsnetto som ger en något yngre befolkning.
* **Låg fertilitet:** Lägre fruktsamhet som dämpar inflödet av unga individer.
* **Stresscenario:** Kombinerar lägre fertilitet, högre dödlighet och lägre migration för att illustrera en ogynnsam demografisk utveckling.

### Sannolikhetsbaserad simulering
Osäkerheten i framskrivningen analyseras genom Monte Carlo-simuleringar där fertilitet, dödlighet och migration tillåts variera slumpmässigt kring historiskt observerade nivåer. Resultaten sammanfattas genom osäkerhetsintervall baserade på 1 000 simuleringar.

## Datakällor
Analysen bygger på öppna data från **Statistiska centralbyrån (SCB)**, hämtade via SCB:s API. Datamaterialet omfattar befolkning efter ålder och kön, åldersspecifik fertilitet, dödlighet samt nettoflyttning.

## Projektstruktur
```text
.
├── Demografi.qmd
├── Befolkning.R
├── Simulering.R
├── index.html
└── README.md
```

## Filbeskrivning

- Demografi.qmd – Quarto-källkod för rapportstrukturen och texten.

- Befolkning.R – Huvudskriptet som hanterar datainhämtning från SCB samt beräkningar för basscenariot och de tre alternativa scenarierna.

- Simulering.R – Fristående beräkningsmotor för den som vill köra om eller testa Monte Carlo-simuleringen (1 000 iterationer) lokalt.

- index.html – Den färdigrenderade, interaktiva rapporten.
