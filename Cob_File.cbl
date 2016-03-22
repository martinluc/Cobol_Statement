   File  Edit  Edit_Settings  Menu  Utilities  Compilers  Test  Help
 -------------------------------------------------------------------------------
 EDIT       ADS02.ADS0205.SRC(ADSOTP5) - 01.99              Columns 00001 00072
 ****** ***************************** Top of Data ******************************
 000001       *===============================================================*
 000002       *--                INFORMATIONS GENERALES                     --*
 000003       *---------------------------------------------------------------*
 000004       *  NOM DU PROGRAMME : XXXXXXXX                                  *
 000005       *  NOM DU REDACTEUR : CREAX                                     *
 000006       *---------------------------------------------------------------*
 000007       *  SOCIETE          : XXXXXXXX                                  *
 000008       *  DATE DE CREATION : JJ/MM/SSAA                                *
 000009       *---------------------------------------------------------------*
 000010       *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
 000011       * REDACTION D UN RELEVE A PARTIR D UN FICHIER.                  *
 000012       *---------------------------------------------------------------*
 000013       *--               HISTORIQUE DES MODIFICATIONS --               *
 000014       *---------------------------------------------------------------*
 000015       * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
 000016       *---------------------------------------------------------------*
 000017       * JJ/MM/SSAA    !                                               *
 000018       *               !                                               *
 000019       *===============================================================*
 000020       *
 000021       *************************
 000022        IDENTIFICATION DIVISION.
 000023       *************************
 000024        PROGRAM-ID.      ADSOTP5.
 000025       *
 000026       *                  ==============================               *
 000027       *=================<  ENVIRONMENT      DIVISION   >==============*
 000028       *                  ==============================               *
 000029       *                                                               *
 000030       *===============================================================*
 000031       *
 000032       **********************
 000033        ENVIRONMENT DIVISION.
 000034       **********************
 000035       *
 000036       *======================
 000037        CONFIGURATION SECTION.
 000038       *======================
 000039       *
 000040       *--------------
 000041        SPECIAL-NAMES.
 000042       *--------------
 000043            DECIMAL-POINT IS COMMA.
 000044       *
 000045       *=====================
 000046        INPUT-OUTPUT SECTION.
 000047       *=====================
 000048       *
 000049       *-------------
 000050        FILE-CONTROL.
 000051       *-------------
 000052       *
 000053       *                      -------------------------------------------
 000054       *                      XXXXXXX : FICHIER DES XXXXX
 000055       *                      -------------------------------------------
 000056            SELECT  FCPTE               ASSIGN TO INP001
 000057                    ORGANIZATION IS INDEXED
 000058                    ACCESS MODE IS DYNAMIC
 000059                    RECORD KEY IS FS-NUM-CPT
 000060                    ALTERNATE RECORD KEY IS FS-NOM-CLI
 000061                                       WITH DUPLICATES
 000062                    FILE STATUS         IS WS-FS-FCPTE.
 000063       *
 000064       *                      -------------------------------------------
 000065       *
 000066            SELECT  ETACLI              ASSIGN TO ETATCLI
 000067                    FILE STATUS         IS WS-FS-ETACLI.
 000068       *
 000069       *                      -------------------------------------------
 000070       *
 000071            SELECT  ETANO               ASSIGN TO ETATANO
 000072                    FILE STATUS         IS WS-FS-ETANO.
 000073       *
 000074       *
 000075       *
 000076       *                  ==============================               *
 000077       *=================<       DATA        DIVISION   >==============*
 000078       *                  ==============================               *
 000079       *                                                               *
 000080       *===============================================================*
 000081       *
 000082       ***************
 000083        DATA DIVISION.
 000084       ***************
 000085       *
 000086       *=============
 000087        FILE SECTION.
 000088       *=============
 000089       *
 000090       *                      -------------------------------------------
 000091       *
 000092        FD  FCPTE.
 000093        01  FS-ENRG-FCPTE.
 000094            05  FS-NUM-CPT        PIC 9(10).
 000095            05  FS-NOM-CLI        PIC X(14).
 000096            05  FILLER            PIC X(26).
 000097       *
 000098       *                      -------------------------------------------
 000099       *
 000100        FD ETACLI.
 000101        01  FS-ENRG-ETACLI        PIC X(78).
 000102       *
 000103       *                      -------------------------------------------
 000104       *
 000105        FD ETANO.
 000106        01  FS-ENRG-ETANO         PIC X(78).
 000107       *
 000108       *                      -------------------------------------------
 000109       *
 000110       *========================
 000111        WORKING-STORAGE SECTION.
 000112       *========================
 000113       *
 000114       *
 000115       *    FILE STATUS DEB   -------------------------------------------
 000116       *
 000117        01  WS-FS-FCPTE     PIC X(2).
 000118            88  FIN-FCPTE               VALUE '10'.
 000119            88  OK-FCPTE                VALUE '00'.
 000120            88  DOUBLON-CP-FCPTE        VALUE '22'.
 000121            88  DOUBLON-CS-FCPTE        VALUE '02'.
 000122            88  ENR-FCPTE-NOK           VALUE '23'.
 000123
 000124        01  WS-FS-ETACLI    PIC X(2).
 000125            88  OK-ETACLI               VALUE '00'.
 000126
 000127        01  WS-FS-ETANO     PIC X(2).
 000128            88  OK-ETANO                VALUE '00'.
 000129       *
 000130       *    FILE STATUS FIN   -------------------------------------------
 000131       *
 000132       *
 000133       *    COMPTEUR DEB      -------------------------------------------
 000134       *
 000135        01  WS-CPT-NB-RQST         PIC 9(6).
 000136
 000137        01  WS-CPT-NUM-RQST        PIC 9(2).
 000138
 000139        01  WS-CPT-NUM-PAGE        PIC 9(3).
 000140
 000141        01  WS-CPT-NB-ERRO         PIC 9(6).
 000142
 000143        01  WS-CPT-NUM-ERR         PIC 99.
 000144
 000145       *
 000146       *    COMPTEUR FIN      -------------------------------------------
 000147       *
 000148       *    AIGUILLAGE DEB    -------------------------------------------
 000149       *
 000150        01  WS-AIG                 PIC 9.
 000151
 000152        01  WS-AIG-TETE-ANO        PIC 9.
 000153
 000154        01  WS-AIG-TETE-CLI        PIC 9.
 000155       *
 000156       *    AIGUILLAGE FIN    -------------------------------------------
 000157       *
 000158       *    FICHIER COPIER DEB   ----------------------------------------
 000159       *
 000160        COPY TP5LEDIT.
 000161
 000162        COPY TP5DEMAN.
 000163
 000164        COPY TP5CPTE.
 000165
 000166       *    FICHIER COPIER FIN   ----------------------------------------
 000167       *
 000168       *
 000169       *                  ==============================               *
 000170       *=================<   PROCEDURE       DIVISION   >==============*
 000171       *                  ==============================               *
 000172       *                                                               *
 000173       *===============================================================*
 000174       *
 000175        PROCEDURE           DIVISION.
 000176       *
 000177       *===============================================================*
 000178       *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
 000179       *---------------------------------------------------------------*
 000180       *                                                               *
 000181       *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
 000182       *        DEUX PARAGRAPHES  XXXX-COMPOSANT-DEB                   *
 000183       *                          XXYY-COMPOSANR-FIN                   *
 000184       *                                                               *
 000185       *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE                   *
 000186       *        YY DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU           *
 000187       *                                                               *
 000188       *    3 : TOUT COMPOSANT EST PRECEDE D'UN CARTOUCHE DE           *
 000189       *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT         *
 000190       *                                                               *
 000191       *                                                               *
 000192       *===============================================================*
 000193       *===============================================================*
 000194       *
 000195       *
 000196       *---------------------------------------------------------------*
 000197       *               DESCRIPTION DU COMPOSANT PROGRAMME              *
 000198       *               ==================================              *
 000199       *---------------------------------------------------------------*
 000200       *
 000201        0000-PROGRAMME-DEB.
 000202       *
 000203       *
 000204       *---------------------------------------------------------------*
 000205       * OREILETTE DE GAUCHE                                           *
 000206       * ===================                                           *
 000207       *---------------------------------------------------------------*
 000208       *
 000209            PERFORM 6000-OPEN-FCPTE-DEB
 000210               THRU 6000-OPEN-FCPTE-FIN.
 000211            PERFORM 6010-OPEN-ETACLI-DEB
 000212               THRU 6010-OPEN-ETACLI-FIN.
 000213            PERFORM 6020-OPEN-ETANO-DEB
 000214               THRU 6020-OPEN-ETANO-FIN.
 000215
 000216            ACCEPT WS-ENR-DEM.
 000217
 000218            INITIALIZE WS-CPT-NB-RQST WS-CPT-NB-ERRO WS-CPT-NUM-RQST
 000219                       WS-CPT-NUM-ERR WS-CPT-NUM-PAGE WS-AIG-TETE-ANO.
 000220
 000221       *
 000222       *---------------------------------------------------------------*
 000223       * APPEL D'UN NOUVEAU COMPOSANT (ITERATION)                      *
 000224       * ========================================                      *
 000225       *---------------------------------------------------------------*
 000226       *
 000227            PERFORM 1000-COMPTE-DEB
 000228               THRU 1000-COMPTE-FIN
 000229               UNTIL WS-DEM-TYP-EOF = '$$$'.
 000230       *
 000231       *---------------------------------------------------------------*
 000232       * OREILETTE DE DROITE                                           *
 000233       * ===================                                           *
 000234       *---------------------------------------------------------------*
 000235       *
 000236
 000237            PERFORM  8020-FDP-CLI-DEB
 000238               THRU  8020-FDP-CLI-FIN.
 000239            PERFORM  8999-STATISTIQUES-DEB
 000240               THRU  8999-STATISTIQUES-FIN.
 000241            PERFORM  8050-FDP-ANO-DEB
 000242               THRU  8050-FDP-ANO-FIN.
 000243
 000244            CLOSE FCPTE.
 000245            CLOSE ETACLI.
 000246            CLOSE ETANO.
 000247       *
 000248            PERFORM  9999-FIN-PROGRAMME-DEB
 000249               THRU  9999-FIN-PROGRAMME-FIN.
 000250       *
 000251        0000-PROGRAMME-FIN.
 000252             EXIT.
 000253       *
 000254       *---------------------------------------------------------------*
 000255       *               DESCRIPTION DU COMPOSANT COMPTE                 *
 000256       *               ===============================                 *
 000257       *---------------------------------------------------------------*
 000258       *
 000259        1000-COMPTE-DEB.
 000260       *
 000261       *
 000262       *---------------------------------------------------------------*
 000263       * OREILETTE DE GAUCHE                                           *
 000264       * ===================                                           *
 000265       *---------------------------------------------------------------*
 000266       *
 000267            MOVE 0 TO WS-AIG.
 000268            MOVE 0 TO WS-AIG-TETE-CLI.
 000269            MOVE WS-DEM-CLI-DEB TO FS-NOM-CLI.
 000270            MOVE WS-DEM-CPT-DEB TO FS-NUM-CPT.
 000271            ADD 1 TO WS-CPT-NUM-RQST.
 000272       *
 000273       *---------------------------------------------------------------*
 000274       * APPEL D'UN NOUVEAU COMPOSANT (ITERATION)                      *
 000275       * ========================================                      *
 000276       *---------------------------------------------------------------*
 000277       *
 000278            EVALUATE TRUE
 000279               WHEN WS-DEM-TYP = 'A'
 000280                      PERFORM 2000-DEMANDE-A-DEB
 000281                         THRU 2000-DEMANDE-A-FIN
 000282               WHEN WS-DEM-TYP = 'B'
 000283                      PERFORM 2010-DEMANDE-B-DEB
 000284                         THRU 2010-DEMANDE-B-FIN
 000285               WHEN OTHER
 000286                      PERFORM 2020-ANOMALIE-DEB
 000287                         THRU 2020-ANOMALIE-FIN
 000288            END-EVALUATE.
 000289
 000290       *---------------------------------------------------------------*
 000291       * OREILETTE DE DROITE                                           *
 000292       * ===================                                           *
 000293       *---------------------------------------------------------------*
 000294       *
 000295            ACCEPT WS-ENR-DEM.
 000296            IF WS-AIG = 1
 000297                ADD 1 TO WS-CPT-NB-RQST
 000298            END-IF.
 000299            IF WS-AIG = 0
 000300                ADD 1 TO WS-CPT-NB-ERRO
 000301            END-IF.
 000302            MOVE 0 TO WS-CPT-NUM-ERR.
 000303
 000304       *
 000305        1000-COMPTE-FIN.
 000306             EXIT.
 000307       *
 000308       *---------------------------------------------------------------*
 000309       *               DESCRIPTION DU COMPOSANT DEMANDE A              *
 000310       *               ==================================              *
 000311       *---------------------------------------------------------------*
 000312       *
 000313        2000-DEMANDE-A-DEB.
 000314       *
 000315       *
 000316       *---------------------------------------------------------------*
 000317       * OREILETTE DE GAUCHE                                           *
 000318       * ===================                                           *
 000319       *---------------------------------------------------------------*
 000320       *
 000321             PERFORM 6030-VERIF-ANO-DEB
 000322                THRU 6030-VERIF-ANO-FIN.
 000323
 000324             IF WS-CPT-NUM-ERR NOT = 1 AND WS-CPT-NUM-ERR NOT = 2
 000325                AND WS-CPT-NUM-ERR NOT = 3
 000326                   ADD 1 TO WS-CPT-NUM-PAGE
 000327                 IF WS-AIG-TETE-CLI = 0
 000328                       PERFORM 8000-TETE-CLI-DEB
 000329                          THRU 8000-TETE-CLI-FIN
 000330                  END-IF
 000331                  MOVE 1 TO  WS-AIG-TETE-CLI
 000332                   START FCPTE KEY >= FS-NUM-CPT
 000333                   IF NOT ENR-FCPTE-NOK
 000334                       READ FCPTE NEXT INTO WS-ENRG-F-CPTE
 000335                   END-IF
 000336
 000337             ELSE
 000338                   PERFORM 2020-ANOMALIE-DEB
 000339                      THRU 2020-ANOMALIE-FIN
 000340             END-IF.
 000341
 000342
 000343
 000344       *
 000345       *---------------------------------------------------------------*
 000346       * APPEL D'UN NOUVEAU COMPOSANT (ITERATION)                      *
 000347       * ========================================                      *
 000348       *---------------------------------------------------------------*
 000349       *
 000350            PERFORM 3000-TRT-A-DEB
 000351               THRU 3000-TRT-A-FIN
 000352               UNTIL WS-CPTE-CPTE > WS-DEM-CPT-FIN OR FIN-FCPTE
 000353                      OR ENR-FCPTE-NOK.
 000354       *
 000355       *---------------------------------------------------------------*
 000356       * OREILETTE DE DROITE                                           *
 000357       * ===================                                           *
 000358       *---------------------------------------------------------------*
 000359       *
 000360             IF WS-CPT-NUM-ERR NOT = 1 AND WS-CPT-NUM-ERR NOT = 2
 000361                AND WS-CPT-NUM-ERR NOT = 3
 000362                       MOVE 1 TO WS-AIG
 000363            END-IF.
 000364       *
 000365        2000-DEMANDE-A-FIN.
 000366             EXIT.
 000367       *
 000368       *
 000369       *---------------------------------------------------------------*
 000370       *               DESCRIPTION DU COMPOSANT DEMANDE B              *
 000371       *               ==================================              *
 000372       *---------------------------------------------------------------*
 000373       *
 000374        2010-DEMANDE-B-DEB.
 000375       *
 000376       *
 000377       *---------------------------------------------------------------*
 000378       * OREILETTE DE GAUCHE                                           *
 000379       * ===================                                           *
 000380       *---------------------------------------------------------------*
 000381       *
 000382             PERFORM 6030-VERIF-ANO-DEB
 000383                THRU 6030-VERIF-ANO-FIN.
 000384             IF WS-CPT-NUM-ERR NOT = 1 AND WS-CPT-NUM-ERR NOT = 2
 000385                AND WS-CPT-NUM-ERR NOT = 3
 000386                      ADD 1 TO WS-CPT-NUM-PAGE
 000387                 IF WS-AIG-TETE-CLI = 0
 000388                       PERFORM 8000-TETE-CLI-DEB
 000389                          THRU 8000-TETE-CLI-FIN
 000390                 END-IF
 000391                 MOVE 1 TO WS-AIG-TETE-CLI
 000392                 START FCPTE KEY >= FS-NOM-CLI
 000393                 IF NOT ENR-FCPTE-NOK
 000394                         READ FCPTE NEXT INTO WS-ENRG-F-CPTE
 000395                 END-IF
 000396              ELSE
 000397                      PERFORM 2020-ANOMALIE-DEB
 000398                         THRU 2020-ANOMALIE-FIN
 000399              END-IF.
 000400
 000401       *
 000402       *---------------------------------------------------------------*
 000403       * APPEL D'UN NOUVEAU COMPOSANT (ITERATION)                      *
 000404       * ========================================                      *
 000405       *---------------------------------------------------------------*
 000406       *
 000407            PERFORM 3010-TRT-B-DEB
 000408               THRU 3010-TRT-B-FIN
 000409               UNTIL (WS-CPTE-NOM > WS-DEM-CLI-FIN OR FIN-FCPTE
 000410                      OR  ENR-FCPTE-NOK).
 000411       *
 000412       *---------------------------------------------------------------*
 000413       * OREILETTE DE DROITE                                           *
 000414       * ===================                                           *
 000415       *---------------------------------------------------------------*
 000416       *
 000417             IF WS-CPT-NUM-ERR NOT = 1 AND WS-CPT-NUM-ERR NOT = 2
 000418                AND WS-CPT-NUM-ERR NOT = 3
 000419                                 MOVE 1 TO WS-AIG
 000420                   END-IF.
 000421       *
 000422        2010-DEMANDE-B-FIN.
 000423             EXIT.
 000424       *
 000425       *---------------------------------------------------------------*
 000426       *               DESCRIPTION DU COMPOSANT ANOMALIE              *
 000427       *               =================================              *
 000428       *---------------------------------------------------------------*
 000429       *
 000430        2020-ANOMALIE-DEB.
 000431       *
 000432       *
 000433       *---------------------------------------------------------------*
 000434       * OREILETTE                                                     *
 000435       * =========                                                     *
 000436       *---------------------------------------------------------------*
 000437       *
 000438             PERFORM 6030-VERIF-ANO-DEB
 000439                THRU 6030-VERIF-ANO-FIN.
 000440
 000441             IF WS-AIG-TETE-ANO = 0
 000442                   PERFORM 8030-TETE-ANO-DEB
 000443                      THRU 8030-TETE-ANO-FIN
 000444             END-IF.
 000445             MOVE 1 TO WS-AIG-TETE-ANO.
 000446
 000447             PERFORM 8040-CORPS-ANO-DEB
 000448                THRU 8040-CORPS-ANO-FIN.
 000449       *
 000450        2020-ANOMALIE-FIN.
 000451             EXIT.
 000452       *
 000453       *
 000454       *---------------------------------------------------------------*
 000455       *               DESCRIPTION DU COMPOSANT TRT A                  *
 000456       *               ==============================                  *
 000457       *---------------------------------------------------------------*
 000458       *
 000459        3000-TRT-A-DEB.
 000460       *
 000461       *
 000462       *---------------------------------------------------------------*
 000463       * OREILETTE DE GAUCHE                                           *
 000464       * ===================                                           *
 000465       *---------------------------------------------------------------*
 000466       *
 000467
 000468             PERFORM 8010-CORPS-CLI-DEB
 000469                THRU 8010-CORPS-CLI-FIN.
 000470             IF NOT ENR-FCPTE-NOK
 000471             READ FCPTE NEXT INTO WS-ENRG-F-CPTE
 000472             END-IF.
 000473       *
 000474       *
 000475        3000-TRT-A-FIN.
 000476             EXIT.
 000477       *
 000478       *
 000479       *---------------------------------------------------------------*
 000480       *               DESCRIPTION DU COMPOSANT TRT B                  *
 000481       *               ==============================                  *
 000482       *---------------------------------------------------------------*
 000483       *
 000484        3010-TRT-B-DEB.
 000485       *
 000486       *
 000487       *---------------------------------------------------------------*
 000488       * OREILETTE DE GAUCHE                                           *
 000489       * ===================                                           *
 000490       *---------------------------------------------------------------*
 000491       *
 000492             PERFORM 8010-CORPS-CLI-DEB
 000493                THRU 8010-CORPS-CLI-FIN.
 000494             IF NOT ENR-FCPTE-NOK
 000495             READ FCPTE NEXT INTO WS-ENRG-F-CPTE
 000496             END-IF.
 000497       *
 000498       *
 000499        3010-TRT-B-FIN.
 000500             EXIT.
 000501       *
 000502       *
 000503       *===============================================================*
 000504       *===============================================================*
 000505       *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
 000506       *---------------------------------------------------------------*
 000507       *                                                               *
 000508       *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
 000509       *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
 000510       *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
 000511       *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
 000512       *   9999-  : PROTECTION FIN DE PROGRAMME                        *
 000513       *                                                               *
 000514       *===============================================================*
 000515       *===============================================================*
 000516       *
 000517       *---------------------------------------------------------------*
 000518       *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
 000519       *---------------------------------------------------------------*
 000520       *                                                               *
 000521       *    OUVERTURE FICHIER DEB  -------------------------------------
 000522       *
 000523        6000-OPEN-FCPTE-DEB.
 000524            OPEN INPUT FCPTE.
 000525            IF NOT OK-FCPTE
 000526               DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER FCPTE'
 000527               DISPLAY 'VALEUR DU FILE STATUS= ' WS-FS-FCPTE
 000528               PERFORM 9999-ERREUR-PROGRAMME-DEB
 000529                  THRU 9999-ERREUR-PROGRAMME-FIN
 000530            END-IF.
 000531        6000-OPEN-FCPTE-FIN.
 000532            EXIT.
 000533
 000534        6010-OPEN-ETACLI-DEB.
 000535            OPEN OUTPUT ETACLI.
 000536            IF NOT OK-ETACLI
 000537               DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER FCPTE'
 000538               DISPLAY 'VALEUR DU FILE STATUS= ' WS-FS-ETACLI
 000539               PERFORM 9999-ERREUR-PROGRAMME-DEB
 000540                  THRU 9999-ERREUR-PROGRAMME-FIN
 000541            END-IF.
 000542        6010-OPEN-ETACLI-FIN.
 000543            EXIT.
 000544
 000545        6020-OPEN-ETANO-DEB.
 000546            OPEN OUTPUT ETANO.
 000547            IF NOT OK-ETANO
 000548               DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER FCPTE'
 000549               DISPLAY 'VALEUR DU FILE STATUS= ' WS-FS-ETANO
 000550               PERFORM 9999-ERREUR-PROGRAMME-DEB
 000551                  THRU 9999-ERREUR-PROGRAMME-FIN
 000552            END-IF.
 000553        6020-OPEN-ETANO-FIN.
 000554            EXIT.
 000555
 000556        6030-VERIF-ANO-DEB.
 000557       *    FAIRE LA VERIF ET METTRE UN MARQUEUR DANS 2000 POUR
 000558       *    SAVOIR SI ON RENTRE DANS UN CAS OU UN AUTRE.
 000559             IF WS-DEM-TYP = 'A'
 000560                  IF WS-DEM-CPT-DEB IS NOT NUMERIC
 000561                     MOVE 3 TO WS-CPT-NUM-ERR
 000562                  END-IF
 000563
 000564                  IF WS-DEM-CPT-FIN IS NOT NUMERIC
 000565                     MOVE 3 TO WS-CPT-NUM-ERR
 000566                  END-IF
 000567
 000568                  IF WS-DEM-CPT-DEB > WS-DEM-CPT-FIN
 000569                     MOVE 2 TO WS-CPT-NUM-ERR
 000570                  END-IF
 000571
 000572                  IF WS-DEM-CPT-DEB = SPACE
 000573                     MOVE 3 TO WS-CPT-NUM-ERR
 000574                  END-IF
 000575
 000576                  IF WS-DEM-CPT-FIN = SPACE
 000577                     MOVE 3 TO WS-CPT-NUM-ERR
 000578                  END-IF
 000579             END-IF.
 000580
 000581       **    IF WS-DEM-NOM = SPACE
 000582       *             MOVE 1 TO WS-CPT-NUM-ERR
 000583       *             WS
 000584       *     END-IF.
 000585
 000586             IF WS-DEM-TYP = 'B'
 000587                  IF WS-DEM-CLI-DEB IS NUMERIC
 000588                     MOVE 3 TO WS-CPT-NUM-ERR
 000589                  END-IF
 000590
 000591                  IF WS-DEM-NOM = SPACE
 000592                    MOVE 3 TO WS-CPT-NUM-ERR
 000593                  END-IF
 000594
 000595                  IF WS-DEM-CLI-FIN IS NUMERIC
 000596                     MOVE 3 TO WS-CPT-NUM-ERR
 000597                  END-IF
 000598
 000599                  IF WS-DEM-CLI-DEB > WS-DEM-CLI-FIN
 000600                     MOVE 2 TO WS-CPT-NUM-ERR
 000601                  END-IF
 000602
 000603                  IF WS-DEM-CLI-DEB = SPACE
 000604                     MOVE 3 TO WS-CPT-NUM-ERR
 000605                  END-IF
 000606
 000607                  IF WS-DEM-CLI-FIN = SPACE
 000608                     MOVE 3 TO WS-CPT-NUM-ERR
 000609                  END-IF
 000610             END-IF.
 000611
 000612             IF WS-DEM-TYP NOT = 'A' AND WS-DEM-TYP NOT = 'B'
 000613                     MOVE 1 TO WS-CPT-NUM-ERR
 000614             END-IF.
 000615
 000616        6030-VERIF-ANO-FIN.
 000617            EXIT.
 000618       *    OUVERTURE FICHIER FIN  -------------------------------------
 000619       *
 000620       *
 000621       *    LECTURE FICHIER DEB  ---------------------------------------
 000622       *
 000623       *6030-READ-FCPTE-DEB.
 000624       *    READ FCPTE INTO WS-ENRG-F-CPTE.
 000625       *    IF NOT (FIN-FCPTE OR OK-FCPTE OR DOUBLON-CS-FCPTE
 000626       *           OR ENR-FCPTE-NOK)
 000627       *       DISPLAY 'PROBLEME DE LECTURE DU FICHIER FCPTE'
 000628       *       DISPLAY 'VALEUR DU FILE STATUS= ' WS-FS-FCPTE
 000629       *       PERFORM 9999-ERREUR-PROGRAMME-DEB
 000630       *          THRU 9999-ERREUR-PROGRAMME-FIN
 000631       *    END-IF.
 000632       *6030-READ-FCPTE-FIN.
 000633       *    EXIT.
 000634       *
 000635       *    LECTURE FICHIER FIN  ---------------------------------------
 000636       *
 000637       *
 000638       *    ECRITURE FICHIER DEB  --------------------------------------
 000639       *
 000640       *
 000641
 000642
 000643       *    FERMETURE FICHIER DEB  -------------------------------------
 000644       *---------------------------------------------------------------*
 000645       *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
 000646       *---------------------------------------------------------------*
 000647       *
 000648       *7000-ORDRE-CALCUL-DEB.
 000649       *
 000650       *7000-ORDRE-CALCUL-FIN.
 000651       *    EXIT.
 000652       *
 000653       *---------------------------------------------------------------*
 000654       *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
 000655       *---------------------------------------------------------------*
 000656       *
 000657       *8000-ORDRE-EDITION-DEB.
 000658       *
 000659       *8000-ORDRE-EDITION-FIN.
 000660       *    EXIT.
 000661       *
 000662       *    EDITION ETATCLI DEB  ---------------------------------------
 000663       *
 000664       *            ENTETE  DEB  ---------------------------------------
 000665       *
 000666        8000-TETE-CLI-DEB.
 000667             WRITE FS-ENRG-ETACLI FROM WS-LETAT-TIRET.
 000668
 000669             MOVE WS-DEM-NOM TO WS-LETAT-NOMD-ED.
 000670             MOVE WS-CPT-NUM-RQST TO WS-LETAT-NUM-ED.
 000671             MOVE WS-CPT-NUM-PAGE TO WS-LETAT-PAGE-ED.
 000672             WRITE FS-ENRG-ETACLI FROM WS-LETAT-ENTETE.
 000673
 000674             WRITE FS-ENRG-ETACLI FROM WS-LETAT-BLANC.
 000675
 000676             IF WS-DEM-TYP = 'A'
 000677                 MOVE 'NUMERO DE COMPTE' TO WS-LETAT-TYPE-ED
 000678                 MOVE WS-DEM-CPT-DEB TO WS-LETAT-REFDEB-ED
 000679                 MOVE WS-DEM-CPT-FIN TO WS-LETAT-REFFIN-ED
 000680             END-IF.
 000681             IF WS-DEM-TYP = 'B'
 000682                 MOVE 'NOM DU CLIENT' TO WS-LETAT-TYPE-ED
 000683                 MOVE WS-DEM-CLI-DEB TO WS-LETAT-REFDEB-ED
 000684                 MOVE WS-DEM-CLI-FIN TO WS-LETAT-REFFIN-ED
 000685             END-IF.
 000686             WRITE FS-ENRG-ETACLI FROM WS-LETAT-TITRE.
 000687
 000688             WRITE FS-ENRG-ETACLI FROM WS-LETAT-BLANC.
 000689
 000690             WRITE FS-ENRG-ETACLI FROM WS-LETAT-REFDEB.
 000691
 000692             WRITE FS-ENRG-ETACLI FROM WS-LETAT-REFFIN.
 000693
 000694             WRITE FS-ENRG-ETACLI FROM WS-LETAT-BLANC.
 000695
 000696             WRITE FS-ENRG-ETACLI FROM WS-LETAT-INTITULE.
 000697
 000698             WRITE FS-ENRG-ETACLI FROM WS-LETAT-BLANC.
 000699        8000-TETE-CLI-FIN.
 000700             EXIT.
 000701       *
 000702       *
 000703       *            ENTETE  FIN  ---------------------------------------
 000704       *
 000705       *
 000706       *            CORPS DEB    ---------------------------------------
 000707       *
 000708       *
 000709        8010-CORPS-CLI-DEB.
 000710
 000711            MOVE WS-CPTE-CPTE TO WS-LETAT-NUMCPT-ED.
 000712            MOVE WS-CPTE-DCREA-SS TO WS-LETAT-DCREA-SS-ED.
 000713            MOVE WS-CPTE-DCREA-AA TO WS-LETAT-DCREA-AA-ED.
 000714            MOVE WS-CPTE-DCREA-MM TO WS-LETAT-DCREA-MM-ED.
 000715            MOVE WS-CPTE-DCREA-JJ TO WS-LETAT-DCREA-JJ-ED.
 000716            MOVE WS-CPTE-DMAJ-SS TO WS-LETAT-DMAJ-SS-ED.
 000717            MOVE WS-CPTE-DMAJ-AA TO WS-LETAT-DMAJ-AA-ED.
 000718            MOVE WS-CPTE-DMAJ-MM TO WS-LETAT-DMAJ-MM-ED.
 000719            MOVE WS-CPTE-DMAJ-JJ TO WS-LETAT-DMAJ-JJ-ED.
 000720            MOVE WS-CPTE-SOLDE TO WS-LETAT-SOLDE-ED.
 000721            MOVE WS-CPTE-NOM TO WS-LETAT-NOMC-ED.
 000722            WRITE FS-ENRG-ETACLI FROM WS-LETAT-DETAIL.
 000723
 000724        8010-CORPS-CLI-FIN.
 000725            EXIT.
 000726       *
 000727       *
 000728       *            CORPS FIN    ---------------------------------------
 000729       *
 000730       *
 000731       *            FIN DE PAGE DEB  -----------------------------------
 000732       *
 000733        8020-FDP-CLI-DEB.
 000734
 000735             WRITE FS-ENRG-ETACLI FROM WS-LETAT-BLANC.
 000736
 000737             WRITE FS-ENRG-ETACLI FROM WS-LETAT-TIRET.
 000738
 000739
 000740        8020-FDP-CLI-FIN.
 000741            EXIT.
 000742       *
 000743       *            FIN DE PAGE FIN  -----------------------------------
 000744       *
 000745       *    EDITION ETATCLI FIN  ---------------------------------------
 000746       *
 000747       *
 000748       *    EDITION ETATANO DEB  ---------------------------------------
 000749       *
 000750       *
 000751       *            ENTETE ANO DEB   -----------------------------------
 000752       *
 000753        8030-TETE-ANO-DEB.
 000754
 000755            WRITE FS-ENRG-ETANO FROM WS-LANO-ASTER.
 000756
 000757            WRITE FS-ENRG-ETANO FROM WS-LANO-TITRE.
 000758
 000759            WRITE FS-ENRG-ETANO FROM WS-LANO-ASTER.
 000760
 000761        8030-TETE-ANO-FIN.
 000762            EXIT.
 000763
 000764       *            ENTETE ANO FIN   -----------------------------------
 000765       *
 000766       *
 000767       *            CORPS ANO DEB    -----------------------------------
 000768       *
 000769        8040-CORPS-ANO-DEB.
 000770
 000771            MOVE WS-CPT-NUM-ERR  TO WS-LANO-NUM-ED.
 000772            IF WS-CPT-NUM-ERR = 1
 000773                 MOVE 'TYPE DE DEMANDE INVALIDE' TO WS-LANO-TYP-ED
 000774            END-IF.
 000775
 000776            IF WS-CPT-NUM-ERR = 2
 000777                 MOVE 'VALEUR DE FIN SUPERIEURE A CELLE DU DEBUT'
 000778                 TO WS-LANO-TYP-ED
 000779            END-IF.
 000780
 000781            IF WS-CPT-NUM-ERR = 3
 000782                 MOVE 'ERREUR SUR LA PLAGE DE VALEUR' TO WS-LANO-TYP-ED
 000783            END-IF.
 000784
 000785            WRITE FS-ENRG-ETANO  FROM WS-LANO-ERREUR.
 000786
 000787            WRITE FS-ENRG-ETANO  FROM WS-LANO-ENR1.
 000788
 000789            MOVE  WS-ENR-DEM     TO WS-LANO-ENR-ED.
 000790            WRITE FS-ENRG-ETANO  FROM WS-LANO-ENR2.
 000791
 000792            WRITE FS-ENRG-ETANO  FROM WS-LANO-INTERL.
 000793
 000794
 000795        8040-CORPS-ANO-FIN.
 000796            EXIT.
 000797       *
 000798       *            CORPS ANO FIN    -----------------------------------
 000799       *
 000800       *
 000801       *            FDP ANO DEB      -----------------------------------
 000802       *
 000803        8050-FDP-ANO-DEB.
 000804
 000805            WRITE FS-ENRG-ETANO  FROM WS-LANO-ASTER.
 000806
 000807        8050-FDP-ANO-FIN.
 000808            EXIT.
 000809       *
 000810       *
 000811       *            FDP ANO DEB      -----------------------------------
 000812       *
 000813       *
 000814       *    EDITION ETATANO DEB  ---------------------------------------
 000815       *
 000816       *
 000817       *    COMPTE RENDU D' EXECUTION DEB  -----------------------------
 000818       *
 000819       *
 000820        8999-STATISTIQUES-DEB.
 000821       *
 000822             DISPLAY '************************************************'
 000823             DISPLAY '*     STATISTIQUES DU PROGRAMME XXXXXXXX       *'
 000824             DISPLAY '*     ==================================       *'
 000825             DISPLAY '************************************************'.
 000826             DISPLAY SPACE
 000827             DISPLAY SPACE
 000828             DISPLAY WS-LCRE-ASTER.
 000829             DISPLAY WS-LCRE-TITRE.
 000830             DISPLAY WS-LCRE-ASTER.
 000831
 000832             MOVE WS-CPT-NB-RQST TO WS-LCRE-DET-TOT-ED.
 000833             MOVE 'NOMBRE DE DEMANDES' TO WS-LCRE-DET-LIB-ED.
 000834             DISPLAY WS-LCRE-DETAIL.
 000835
 000836             MOVE WS-CPT-NB-ERRO TO WS-LCRE-DET-TOT-ED.
 000837             MOVE 'NOMBRE DE DEMANDES ERRONEES' TO WS-LCRE-DET-LIB-ED.
 000838             DISPLAY WS-LCRE-DETAIL.
 000839
 000840             DISPLAY WS-LCRE-ASTER.
 000841       *
 000842        8999-STATISTIQUES-FIN.
 000843             EXIT.
 000844       *
 000845       *    COMPTE RENDU D' EXECUTION FIN  -----------------------------
 000846       *
 000847       *---------------------------------------------------------------*
 000848       *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
 000849       *---------------------------------------------------------------*
 000850       *
 000851       *9000-APPEL-SP-DEB.
 000852       *
 000853       *9000-APPEL-SP-FIN.
 000854       *    EXIT.
 000855       *
 000856       *---------------------------------------------------------------*
 000857       *   9999-  : PROTECTION FIN DE PROGRAMME                        *
 000858       *---------------------------------------------------------------*
 000859       *
 000860        9999-FIN-PROGRAMME-DEB.
 000861       *
 000862             DISPLAY '*==============================================*'
 000863             DISPLAY '*     FIN NORMALE DU PROGRAMME XXXXXXXX        *'
 000864             DISPLAY '*==============================================*'.
 000865       *
 000866        9999-FIN-PROGRAMME-FIN.
 000867             GOBACK.
 000868       *
 000869        9999-ERREUR-PROGRAMME-DEB.
 000870       *
 000871             DISPLAY '*==============================================*'
 000872             DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'
 000873             DISPLAY '*     FIN ANORMALE DU PROGRAMME XXXXXXXX       *'
 000874             DISPLAY '*==============================================*'.
 000875       *
 000876        9999-ERREUR-PROGRAMME-FIN.
 000877             STOP RUN.
 ****** **************************** Bottom of Data ****************************

