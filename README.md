[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/rq8z89eH)
[![Open in Codespaces](https://classroom.github.com/assets/launch-codespace-2972f46106e565e64193e422d61a12cf1da4916b45550586e14ef0a7c637dd04.svg)](https://classroom.github.com/open-in-codespaces?assignment_repo_id=18925582)
# Uzdevums
 Uzrakstiet programmu R valodā, kura ielasa un apstrādā datus no teksta datnes.

## Uzdevuma variants ir atlikums no Jūsu studentu apliecības numura dalīšanas ar 20
 Piemēram, ja Jūsu apliecības numurs ir 211RXY036, tad Jums ir 16. variants, t.i., ir jāizmanto fails "variants16.txt".

 Ja Jums ir 0. variants, izmantojiet failu "variants20.txt".
 
 Jums ir jāizdzēš visi pārējie faili variantsNN.txt, un ir jāatstāj tikai savam variantam atbilstošais.

 Programmai ir jāveic šādas darbības:
 1. ielasa teksta datni (nosaukums atbilstoši variantam) un saglabā to datu satvarā kordat;
 2. pārveido kolonnas no 9. un uz augšu par faktoriem;
 3. turpmāko izvadu nodrošina uz teksta datni results.txt;
 4. izvada šajā datnē kopsavilkumu, cik ieraksti ir atrodami katrā no faktoru līmeņiem (tie arī ir kolonnās no 9. un uz augšu).
   Līmenis ir burta un ciparu kombinācija, kuras atdala punkti; piem., a7 un x3 abi ir līmeņi, pirmais ir a faktora 8. līmenis un otrais ir x faktora 4. līmenis.
 5. saskalda kordat kolonnu "Slope" sarakstā pēc b faktora vērtībām un saglabā to mainīgajā sl.by.b, kura saturu izdrukā datnē results.txt;
 6. izveido kolonnu ar nosaukumu "Average", kurā saglabā aritmētisko vidējo no kolonnām "Slope", "Intercept" un "adj.r.squared";
 7. aprēķina standartnovirzi pa f faktora līmeņiem (visām kolonnām) un izvada to datnē;
 8. nokopē no kordat uz citu datu satvaru ar nosaukumu prockordat tikai tās rindiņas, kurām vērtība "adj.r.squared" kolonnā ir lielāka par 0.7 (ja kolonnā ir pozitīvas vērtības) vai lielāka par -0.3 (ja vērtības kolonnā ir negatīvas);
 9. prockordat pārraksta "Slope" kolonnas vērtību, ierakstot šūnās formulas 1-1/k rezultātu, kur k ir "Slope" sākotnējā vērtība;
 10. izdrukā prockordat datnē results.txt;
 11. uzzīmē izkliedes grafiku kur uz horizontālās ass ir kolonnas "MAD" vērtībus, bet uz vertikālās – kolonnas "Average" vērtības no datu satvara kordat;
     Grafiks ir jāsaglabā datnē scatter.svg .
 13. uzzīmē vienā attēlā sagrupētu kastu grafiku no "Intercept" kolonnas datiem no datu satvara kordat, grupējot pēc f faktora vērtībām un variējot pēc tām arī kastes izskatu.
     Grafiks ir jāsaglabā datnē boxplot.svg .
 
 Programmai ir jāatbilst vairākiem kritērijiem:
 * ielasot datni, ir jāattīra dati no nejaušām atstarpēm, kas liedz korekti pārveidot kolonnas par faktoriem (ielasīšanas funkcijai ir attiecīgais arguments);
 * pirmā kolonna datnē ir jāpārveido par datu satvara kordat rindu nosaukumiem.
 
 Ja Jums kaut kas nesanāk – izdariet to, kas sanāk; piemēram, ja nezināt, kā lai 7. punktā izdrukā standartnovirzi pa f līmeņiem visām kolonnām, izdariet to jebkurai vienai kolonnai.
 
 Papilduzdevums: atrast, kurš faktora līmenis (skat. 4. punktu uzdevumos) kordat rindu nosakumos ir sastopams visbiežāk, un izvadīt uz ekrāna tikai tās prockordat rindiņas, kuru nosaukumi satur šo līmeni.
 
 Programma ir jāieraksta failā programma.R .

 Beigās saglabājiet iesniedziet Jūsu mainīto failu ar commit (aprakstā norādiet, kas tika izdarīts) un iesniedziet to ORTUS.
 
 ORTUS Jums būs jāiesniedz programma.R datne.
