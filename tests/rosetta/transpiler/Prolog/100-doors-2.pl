:- initialization(main).
:- style_check(-singleton).

main :-
    Door is 1,
    Incrementer is 0,
    Current is 1,
    Line = "Door 1 ",
    (true ->
    Line1 = "Door 1 Open",
    Incrementer1 is 1,
    Door1 is 4 ;
    Line2 = "Door 1 OpenClosed"),
    writeln("Door 1 OpenClosed"),
    Current1 is 2,
    Line3 = "Door 2 ",
    (false ->
    Line4 = "Door 2 Open",
    Incrementer2 is 2,
    Door2 is 13 ;
    Line5 = "Door 2 OpenClosed"),
    writeln("Door 2 OpenClosed"),
    Current2 is 3,
    Line6 = "Door 3 ",
    (false ->
    Line7 = "Door 3 Open",
    Incrementer3 is 3,
    Door3 is 46 ;
    Line8 = "Door 3 OpenClosed"),
    writeln("Door 3 OpenClosed"),
    Current3 is 4,
    Line9 = "Door 4 ",
    (false ->
    Line10 = "Door 4 Open",
    Incrementer4 is 4,
    Door4 is 193 ;
    Line11 = "Door 4 OpenClosed"),
    writeln("Door 4 OpenClosed"),
    Current4 is 5,
    Line12 = "Door 5 ",
    (false ->
    Line13 = "Door 5 Open",
    Incrementer5 is 5,
    Door5 is 976 ;
    Line14 = "Door 5 OpenClosed"),
    writeln("Door 5 OpenClosed"),
    Current5 is 6,
    Line15 = "Door 6 ",
    (false ->
    Line16 = "Door 6 Open",
    Incrementer6 is 6,
    Door6 is 5869 ;
    Line17 = "Door 6 OpenClosed"),
    writeln("Door 6 OpenClosed"),
    Current6 is 7,
    Line18 = "Door 7 ",
    (false ->
    Line19 = "Door 7 Open",
    Incrementer7 is 7,
    Door7 is 41098 ;
    Line20 = "Door 7 OpenClosed"),
    writeln("Door 7 OpenClosed"),
    Current7 is 8,
    Line21 = "Door 8 ",
    (false ->
    Line22 = "Door 8 Open",
    Incrementer8 is 8,
    Door8 is 328801 ;
    Line23 = "Door 8 OpenClosed"),
    writeln("Door 8 OpenClosed"),
    Current8 is 9,
    Line24 = "Door 9 ",
    (false ->
    Line25 = "Door 9 Open",
    Incrementer9 is 9,
    Door9 is 2959228 ;
    Line26 = "Door 9 OpenClosed"),
    writeln("Door 9 OpenClosed"),
    Current9 is 10,
    Line27 = "Door 10 ",
    (false ->
    Line28 = "Door 10 Open",
    Incrementer10 is 10,
    Door10 is 29592301 ;
    Line29 = "Door 10 OpenClosed"),
    writeln("Door 10 OpenClosed"),
    Current10 is 11,
    Line30 = "Door 11 ",
    (false ->
    Line31 = "Door 11 Open",
    Incrementer11 is 11,
    Door11 is 325515334 ;
    Line32 = "Door 11 OpenClosed"),
    writeln("Door 11 OpenClosed"),
    Current11 is 12,
    Line33 = "Door 12 ",
    (false ->
    Line34 = "Door 12 Open",
    Incrementer12 is 12,
    Door12 is 3906184033 ;
    Line35 = "Door 12 OpenClosed"),
    writeln("Door 12 OpenClosed"),
    Current12 is 13,
    Line36 = "Door 13 ",
    (false ->
    Line37 = "Door 13 Open",
    Incrementer13 is 13,
    Door13 is 50780392456 ;
    Line38 = "Door 13 OpenClosed"),
    writeln("Door 13 OpenClosed"),
    Current13 is 14,
    Line39 = "Door 14 ",
    (false ->
    Line40 = "Door 14 Open",
    Incrementer14 is 14,
    Door14 is 710925494413 ;
    Line41 = "Door 14 OpenClosed"),
    writeln("Door 14 OpenClosed"),
    Current14 is 15,
    Line42 = "Door 15 ",
    (false ->
    Line43 = "Door 15 Open",
    Incrementer15 is 15,
    Door15 is 10663882416226 ;
    Line44 = "Door 15 OpenClosed"),
    writeln("Door 15 OpenClosed"),
    Current15 is 16,
    Line45 = "Door 16 ",
    (false ->
    Line46 = "Door 16 Open",
    Incrementer16 is 16,
    Door16 is 170622118659649 ;
    Line47 = "Door 16 OpenClosed"),
    writeln("Door 16 OpenClosed"),
    Current16 is 17,
    Line48 = "Door 17 ",
    (false ->
    Line49 = "Door 17 Open",
    Incrementer17 is 17,
    Door17 is 2900576017214068 ;
    Line50 = "Door 17 OpenClosed"),
    writeln("Door 17 OpenClosed"),
    Current17 is 18,
    Line51 = "Door 18 ",
    (false ->
    Line52 = "Door 18 Open",
    Incrementer18 is 18,
    Door18 is 52210368309853261 ;
    Line53 = "Door 18 OpenClosed"),
    writeln("Door 18 OpenClosed"),
    Current18 is 19,
    Line54 = "Door 19 ",
    (false ->
    Line55 = "Door 19 Open",
    Incrementer19 is 19,
    Door19 is 991996997887211998 ;
    Line56 = "Door 19 OpenClosed"),
    writeln("Door 19 OpenClosed"),
    Current19 is 20,
    Line57 = "Door 20 ",
    (false ->
    Line58 = "Door 20 Open",
    Incrementer20 is 20,
    Door20 is 1393195884034688385 ;
    Line59 = "Door 20 OpenClosed"),
    writeln("Door 20 OpenClosed"),
    Current20 is 21,
    Line60 = "Door 21 ",
    (false ->
    Line61 = "Door 21 Open",
    Incrementer21 is 21,
    Door21 is -7636374582690647104 ;
    Line62 = "Door 21 OpenClosed"),
    writeln("Door 21 OpenClosed"),
    Current21 is 22,
    Line63 = "Door 22 ",
    (false ->
    Line64 = "Door 22 Open",
    Incrementer22 is 22,
    Door22 is -1979544155808271699 ;
    Line65 = "Door 22 OpenClosed"),
    writeln("Door 22 OpenClosed"),
    Current22 is 23,
    Line66 = "Door 23 ",
    (false ->
    Line67 = "Door 23 Open",
    Incrementer23 is 23,
    Door23 is -8636027436171145798 ;
    Line68 = "Door 23 OpenClosed"),
    writeln("Door 23 OpenClosed"),
    Current23 is 24,
    Line69 = "Door 24 ",
    (false ->
    Line70 = "Door 24 Open",
    Incrementer24 is 24,
    Door24 is -4350473657302431327 ;
    Line71 = "Door 24 OpenClosed"),
    writeln("Door 24 OpenClosed"),
    Current24 is 25,
    Line72 = "Door 25 ",
    (false ->
    Line73 = "Door 25 Open",
    Incrementer25 is 25,
    Door25 is 1918623009696526572 ;
    Line74 = "Door 25 OpenClosed"),
    writeln("Door 25 OpenClosed"),
    Current25 is 26,
    Line75 = "Door 26 ",
    (false ->
    Line76 = "Door 26 Open",
    Incrementer26 is 26,
    Door26 is -5456033969018963923 ;
    Line77 = "Door 26 OpenClosed"),
    writeln("Door 26 OpenClosed"),
    Current26 is 27,
    Line78 = "Door 27 ",
    (false ->
    Line79 = "Door 27 Open",
    Incrementer27 is 27,
    Door27 is 261035426164387062 ;
    Line80 = "Door 27 OpenClosed"),
    writeln("Door 27 OpenClosed"),
    Current27 is 28,
    Line81 = "Door 28 ",
    (false ->
    Line82 = "Door 28 Open",
    Incrementer28 is 28,
    Door28 is 7308991932602837793 ;
    Line83 = "Door 28 OpenClosed"),
    writeln("Door 28 OpenClosed"),
    Current28 is 29,
    Line84 = "Door 29 ",
    (false ->
    Line85 = "Door 29 Open",
    Incrementer29 is 29,
    Door29 is 9046581234677228280 ;
    Line86 = "Door 29 OpenClosed"),
    writeln("Door 29 OpenClosed"),
    Current29 is 30,
    Line87 = "Door 30 ",
    (false ->
    Line88 = "Door 30 Open",
    Incrementer30 is 30,
    Door30 is -5303724065326425779 ;
    Line89 = "Door 30 OpenClosed"),
    writeln("Door 30 OpenClosed"),
    Current30 is 31,
    Line90 = "Door 31 ",
    (false ->
    Line91 = "Door 31 Open",
    Incrementer31 is 31,
    Door31 is 1605250638266765458 ;
    Line92 = "Door 31 OpenClosed"),
    writeln("Door 31 OpenClosed"),
    Current31 is 32,
    Line93 = "Door 32 ",
    (false ->
    Line94 = "Door 32 Open",
    Incrementer32 is 32,
    Door32 is -3972211796592160127 ;
    Line95 = "Door 32 OpenClosed"),
    writeln("Door 32 OpenClosed"),
    Current32 is 33,
    Line96 = "Door 33 ",
    (false ->
    Line97 = "Door 33 Open",
    Incrementer33 is 33,
    Door33 is -1955780771574422812 ;
    Line98 = "Door 33 OpenClosed"),
    writeln("Door 33 OpenClosed"),
    Current33 is 34,
    Line99 = "Door 34 ",
    (false ->
    Line100 = "Door 34 Open",
    Incrementer34 is 34,
    Door34 is 7290430061307830925 ;
    Line101 = "Door 34 OpenClosed"),
    writeln("Door 34 OpenClosed"),
    Current34 is 35,
    Line102 = "Door 35 ",
    (false ->
    Line103 = "Door 35 Open",
    Incrementer35 is 35,
    Door35 is -3089364886159640178 ;
    Line104 = "Door 35 OpenClosed"),
    writeln("Door 35 OpenClosed"),
    Current35 is 36,
    Line105 = "Door 36 ",
    (false ->
    Line106 = "Door 36 Open",
    Incrementer36 is 36,
    Door36 is -536671459489736639 ;
    Line107 = "Door 36 OpenClosed"),
    writeln("Door 36 OpenClosed"),
    Current36 is 37,
    Line108 = "Door 37 ",
    (false ->
    Line109 = "Door 37 Open",
    Incrementer37 is 37,
    Door37 is -1410099927410703952 ;
    Line110 = "Door 37 OpenClosed"),
    writeln("Door 37 OpenClosed"),
    Current37 is 38,
    Line111 = "Door 38 ",
    (false ->
    Line112 = "Door 38 Open",
    Incrementer38 is 38,
    Door38 is 1756434979521904749 ;
    Line113 = "Door 38 OpenClosed"),
    writeln("Door 38 OpenClosed"),
    Current38 is 39,
    Line114 = "Door 39 ",
    (false ->
    Line115 = "Door 39 Open",
    Incrementer39 is 39,
    Door39 is -5286012093483921174 ;
    Line116 = "Door 39 OpenClosed"),
    writeln("Door 39 OpenClosed"),
    Current39 is 40,
    Line117 = "Door 40 ",
    (false ->
    Line118 = "Door 40 Open",
    Incrementer40 is 40,
    Door40 is -8526298928551779103 ;
    Line119 = "Door 40 OpenClosed"),
    writeln("Door 40 OpenClosed"),
    Current40 is 41,
    Line120 = "Door 41 ",
    (false ->
    Line121 = "Door 41 Open",
    Incrementer41 is 41,
    Door41 is 909881329858537564 ;
    Line122 = "Door 41 OpenClosed"),
    writeln("Door 41 OpenClosed"),
    Current41 is 42,
    Line123 = "Door 42 ",
    (false ->
    Line124 = "Door 42 Open",
    Incrementer42 is 42,
    Door42 is 1321527706639474541 ;
    Line125 = "Door 42 OpenClosed"),
    writeln("Door 42 OpenClosed"),
    Current42 is 43,
    Line126 = "Door 43 ",
    (false ->
    Line127 = "Door 43 Open",
    Incrementer43 is 43,
    Door43 is 1485459164368750502 ;
    Line128 = "Door 43 OpenClosed"),
    writeln("Door 43 OpenClosed"),
    Current43 is 44,
    Line129 = "Door 44 ",
    (false ->
    Line130 = "Door 44 Open",
    Incrementer44 is 44,
    Door44 is -8426773062613184287 ;
    Line131 = "Door 44 OpenClosed"),
    writeln("Door 44 OpenClosed"),
    Current44 is 45,
    Line132 = "Door 45 ",
    (false ->
    Line133 = "Door 45 Open",
    Incrementer45 is 45,
    Door45 is 8176837730307291112 ;
    Line134 = "Door 45 OpenClosed"),
    writeln("Door 45 OpenClosed"),
    Current45 is 46,
    Line135 = "Door 46 ",
    (false ->
    Line136 = "Door 46 Open",
    Incrementer46 is 46,
    Door46 is 7199654119944358925 ;
    Line137 = "Door 46 OpenClosed"),
    writeln("Door 46 OpenClosed"),
    Current46 is 47,
    Line138 = "Door 47 ",
    (false ->
    Line139 = "Door 47 Open",
    Incrementer47 is 47,
    Door47 is 6342350310612940482 ;
    Line140 = "Door 47 OpenClosed"),
    writeln("Door 47 OpenClosed"),
    Current47 is 48,
    Line141 = "Door 48 ",
    (false ->
    Line142 = "Door 48 Open",
    Incrementer48 is 48,
    Door48 is -9161834343641234239 ;
    Line143 = "Door 48 OpenClosed"),
    writeln("Door 48 OpenClosed"),
    Current48 is 49,
    Line144 = "Door 49 ",
    (false ->
    Line145 = "Door 49 Open",
    Incrementer49 is 49,
    Door49 is -6208025069391238828 ;
    Line146 = "Door 49 OpenClosed"),
    writeln("Door 49 OpenClosed"),
    Current49 is 50,
    Line147 = "Door 50 ",
    (false ->
    Line148 = "Door 50 Open",
    Incrementer50 is 50,
    Door50 is 3193395783500436173 ;
    Line149 = "Door 50 OpenClosed"),
    writeln("Door 50 OpenClosed"),
    Current50 is 51,
    Line150 = "Door 51 ",
    (false ->
    Line151 = "Door 51 Open",
    Incrementer51 is 51,
    Door51 is -3157511704863719618 ;
    Line152 = "Door 51 OpenClosed"),
    writeln("Door 51 OpenClosed"),
    Current51 is 52,
    Line153 = "Door 52 ",
    (false ->
    Line154 = "Door 52 Open",
    Incrementer52 is 52,
    Door52 is 1830088010472544513 ;
    Line155 = "Door 52 OpenClosed"),
    writeln("Door 52 OpenClosed"),
    Current52 is 53,
    Line156 = "Door 53 ",
    (false ->
    Line157 = "Door 53 Open",
    Incrementer53 is 53,
    Door53 is 4760944186497101216 ;
    Line158 = "Door 53 OpenClosed"),
    writeln("Door 53 OpenClosed"),
    Current53 is 54,
    Line159 = "Door 54 ",
    (false ->
    Line160 = "Door 54 Open",
    Incrementer54 is 54,
    Door54 is -1163430961090256851 ;
    Line161 = "Door 54 OpenClosed"),
    writeln("Door 54 OpenClosed"),
    Current54 is 55,
    Line162 = "Door 55 ",
    (false ->
    Line163 = "Door 55 Open",
    Incrementer55 is 55,
    Door55 is -8648470638835471846 ;
    Line164 = "Door 55 OpenClosed"),
    writeln("Door 55 OpenClosed"),
    Current55 is 56,
    Line165 = "Door 56 ",
    (false ->
    Line166 = "Door 56 Open",
    Incrementer56 is 56,
    Door56 is -4699009858338081247 ;
    Line167 = "Door 56 OpenClosed"),
    writeln("Door 56 OpenClosed"),
    Current56 is 57,
    Line168 = "Door 57 ",
    (false ->
    Line169 = "Door 57 Open",
    Incrementer57 is 57,
    Door57 is 8857599180372643276 ;
    Line170 = "Door 57 OpenClosed"),
    writeln("Door 57 OpenClosed"),
    Current57 is 58,
    Line171 = "Door 58 ",
    (false ->
    Line172 = "Door 58 Open",
    Incrementer58 is 58,
    Door58 is -2768081602254135123 ;
    Line173 = "Door 58 OpenClosed"),
    writeln("Door 58 OpenClosed"),
    Current58 is 59,
    Line174 = "Door 59 ",
    (false ->
    Line175 = "Door 59 Open",
    Incrementer59 is 59,
    Door59 is 2703882130391992406 ;
    Line176 = "Door 59 OpenClosed"),
    writeln("Door 59 OpenClosed"),
    Current59 is 60,
    Line177 = "Door 60 ",
    (false ->
    Line178 = "Door 60 Open",
    Incrementer60 is 60,
    Door60 is -3787768839866420063 ;
    Line179 = "Door 60 OpenClosed"),
    writeln("Door 60 OpenClosed"),
    Current60 is 61,
    Line180 = "Door 61 ",
    (false ->
    Line181 = "Door 61 Open",
    Incrementer61 is 61,
    Door61 is 8753773726372547288 ;
    Line182 = "Door 61 OpenClosed"),
    writeln("Door 61 OpenClosed"),
    Current61 is 62,
    Line183 = "Door 62 ",
    (false ->
    Line184 = "Door 62 Open",
    Incrementer62 is 62,
    Door62 is 7778392897520935117 ;
    Line185 = "Door 62 OpenClosed"),
    writeln("Door 62 OpenClosed"),
    Current62 is 63,
    Line186 = "Door 63 ",
    (false ->
    Line187 = "Door 63 Open",
    Incrementer63 is 63,
    Door63 is -8023337446338981134 ;
    Line188 = "Door 63 OpenClosed"),
    writeln("Door 63 OpenClosed"),
    Current63 is 64,
    Line189 = "Door 64 ",
    (false ->
    Line190 = "Door 64 Open",
    Incrementer64 is 64,
    Door64 is 3015237498172652801 ;
    Line191 = "Door 64 OpenClosed"),
    writeln("Door 64 OpenClosed"),
    Current64 is 65,
    Line192 = "Door 65 ",
    (false ->
    Line193 = "Door 65 Open",
    Incrementer65 is 65,
    Door65 is -6923747429582635580 ;
    Line194 = "Door 65 OpenClosed"),
    writeln("Door 65 OpenClosed"),
    Current65 is 66,
    Line195 = "Door 66 ",
    (false ->
    Line196 = "Door 66 Open",
    Incrementer66 is 66,
    Door66 is 4201271490284842253 ;
    Line197 = "Door 66 OpenClosed"),
    writeln("Door 66 OpenClosed"),
    Current66 is 67,
    Line198 = "Door 67 ",
    (false ->
    Line199 = "Door 67 Open",
    Incrementer67 is 67,
    Door67 is 4784028743441156846 ;
    Line200 = "Door 67 OpenClosed"),
    writeln("Door 67 OpenClosed"),
    Current67 is 68,
    Line201 = "Door 68 ",
    (false ->
    Line202 = "Door 68 Open",
    Incrementer68 is 68,
    Door68 is -6727438772773263423 ;
    Line203 = "Door 68 OpenClosed"),
    writeln("Door 68 OpenClosed"),
    Current68 is 69,
    Line204 = "Door 69 ",
    (false ->
    Line205 = "Door 69 Open",
    Incrementer69 is 69,
    Door69 is -3024673478616385648 ;
    Line206 = "Door 69 OpenClosed"),
    writeln("Door 69 OpenClosed"),
    Current69 is 70,
    Line207 = "Door 70 ",
    (false ->
    Line208 = "Door 70 Open",
    Incrementer70 is 70,
    Door70 is -8812958692341927443 ;
    Line209 = "Door 70 OpenClosed"),
    writeln("Door 70 OpenClosed"),
    Current70 is 71,
    Line210 = "Door 71 ",
    (false ->
    Line211 = "Door 71 Open",
    Incrementer71 is 71,
    Door71 is 1469231349847906634 ;
    Line212 = "Door 71 OpenClosed"),
    writeln("Door 71 OpenClosed"),
    Current71 is 72,
    Line213 = "Door 72 ",
    (false ->
    Line214 = "Door 72 Open",
    Incrementer72 is 72,
    Door72 is -4895807253208031903 ;
    Line215 = "Door 72 OpenClosed"),
    writeln("Door 72 OpenClosed"),
    Current72 is 73,
    Line216 = "Door 73 ",
    (false ->
    Line217 = "Door 73 Open",
    Incrementer73 is 73,
    Door73 is -6905792083704848068 ;
    Line218 = "Door 73 OpenClosed"),
    writeln("Door 73 OpenClosed"),
    Current73 is 74,
    Line219 = "Door 74 ",
    (false ->
    Line220 = "Door 74 Open",
    Incrementer74 is 74,
    Door74 is 5480219869708688365 ;
    Line221 = "Door 74 OpenClosed"),
    writeln("Door 74 OpenClosed"),
    Current74 is 75,
    Line222 = "Door 75 ",
    (false ->
    Line223 = "Door 75 Open",
    Incrementer75 is 75,
    Door75 is 5188120606541491974 ;
    Line224 = "Door 75 OpenClosed"),
    writeln("Door 75 OpenClosed"),
    Current75 is 76,
    Line225 = "Door 76 ",
    (false ->
    Line226 = "Door 76 Open",
    Incrementer76 is 76,
    Door76 is 6915540549252806241 ;
    Line227 = "Door 76 OpenClosed"),
    writeln("Door 76 OpenClosed"),
    Current76 is 77,
    Line228 = "Door 77 ",
    (false ->
    Line229 = "Door 77 Open",
    Incrementer77 is 77,
    Door77 is -2458955845110916152 ;
    Line230 = "Door 77 OpenClosed"),
    writeln("Door 77 OpenClosed"),
    Current77 is 78,
    Line231 = "Door 78 ",
    (false ->
    Line232 = "Door 78 Open",
    Incrementer78 is 78,
    Door78 is -7331115181555943539 ;
    Line233 = "Door 78 OpenClosed"),
    writeln("Door 78 OpenClosed"),
    Current78 is 79,
    Line234 = "Door 79 ",
    (false ->
    Line235 = "Door 79 Open",
    Incrementer79 is 79,
    Door79 is -7309033057923439326 ;
    Line236 = "Door 79 OpenClosed"),
    writeln("Door 79 OpenClosed"),
    Current79 is 80,
    Line237 = "Door 80 ",
    (false ->
    Line238 = "Door 80 Open",
    Incrementer80 is 80,
    Door80 is 5573165724830505793 ;
    Line239 = "Door 80 OpenClosed"),
    writeln("Door 80 OpenClosed"),
    Current80 is 81,
    Line240 = "Door 81 ",
    (false ->
    Line241 = "Door 81 Open",
    Incrementer81 is 81,
    Door81 is 8704565942241730612 ;
    Line242 = "Door 81 OpenClosed"),
    writeln("Door 81 OpenClosed"),
    Current81 is 82,
    Line243 = "Door 82 ",
    (false ->
    Line244 = "Door 82 Open",
    Incrementer82 is 82,
    Door82 is -5648611610850602675 ;
    Line245 = "Door 82 OpenClosed"),
    writeln("Door 82 OpenClosed"),
    Current82 is 83,
    Line246 = "Door 83 ",
    (false ->
    Line247 = "Door 83 Open",
    Incrementer83 is 83,
    Door83 is -7666161857861231458 ;
    Line248 = "Door 83 OpenClosed"),
    writeln("Door 83 OpenClosed"),
    Current83 is 84,
    Line249 = "Door 84 ",
    (false ->
    Line250 = "Door 84 Open",
    Incrementer84 is 84,
    Door84 is 1678446519490864257 ;
    Line251 = "Door 84 OpenClosed"),
    writeln("Door 84 OpenClosed"),
    Current84 is 85,
    Line252 = "Door 85 ",
    (false ->
    Line253 = "Door 85 Open",
    Incrementer85 is 85,
    Door85 is -4905998432952950912 ;
    Line254 = "Door 85 OpenClosed"),
    writeln("Door 85 OpenClosed"),
    Current85 is 86,
    Line255 = "Door 86 ",
    (false ->
    Line256 = "Door 86 Open",
    Incrementer86 is 86,
    Door86 is 2359248461365908909 ;
    Line257 = "Door 86 OpenClosed"),
    writeln("Door 86 OpenClosed"),
    Current86 is 87,
    Line258 = "Door 87 ",
    (false ->
    Line259 = "Door 87 Open",
    Incrementer87 is 87,
    Door87 is 2340431328029007482 ;
    Line260 = "Door 87 OpenClosed"),
    writeln("Door 87 OpenClosed"),
    Current87 is 88,
    Line261 = "Door 88 ",
    (false ->
    Line262 = "Door 88 Open",
    Incrementer88 is 88,
    Door88 is 3043772055747590817 ;
    Line263 = "Door 88 OpenClosed"),
    writeln("Door 88 OpenClosed"),
    Current88 is 89,
    Line264 = "Door 89 ",
    (false ->
    Line265 = "Door 89 Open",
    Incrementer89 is 89,
    Door89 is -5805448144107691348 ;
    Line266 = "Door 89 OpenClosed"),
    writeln("Door 89 OpenClosed"),
    Current89 is 90,
    Line267 = "Door 90 ",
    (false ->
    Line268 = "Door 90 Open",
    Incrementer90 is 90,
    Door90 is -5981498905824775891 ;
    Line269 = "Door 90 OpenClosed"),
    writeln("Door 90 OpenClosed"),
    Current90 is 91,
    Line270 = "Door 91 ",
    (false ->
    Line271 = "Door 91 Open",
    Incrementer91 is 91,
    Door91 is 9085921781231942582 ;
    Line272 = "Door 91 OpenClosed"),
    writeln("Door 91 OpenClosed"),
    Current91 is 92,
    Line273 = "Door 92 ",
    (false ->
    Line274 = "Door 92 Open",
    Incrementer92 is 92,
    Door92 is 5801320556408895009 ;
    Line275 = "Door 92 OpenClosed"),
    writeln("Door 92 OpenClosed"),
    Current92 is 93,
    Line276 = "Door 93 ",
    (false ->
    Line277 = "Door 93 Open",
    Incrementer93 is 93,
    Door93 is 4567233608450239160 ;
    Line278 = "Door 93 OpenClosed"),
    writeln("Door 93 OpenClosed"),
    Current93 is 94,
    Line279 = "Door 94 ",
    (false ->
    Line280 = "Door 94 Open",
    Incrementer94 is 94,
    Door94 is 5044845499002794061 ;
    Line281 = "Door 94 OpenClosed"),
    writeln("Door 94 OpenClosed"),
    Current94 is 95,
    Line282 = "Door 95 ",
    (false ->
    Line283 = "Door 95 Open",
    Incrementer95 is 95,
    Door95 is -355023511182906030 ;
    Line284 = "Door 95 OpenClosed"),
    writeln("Door 95 OpenClosed"),
    Current95 is 96,
    Line285 = "Door 96 ",
    (false ->
    Line286 = "Door 96 Open",
    Incrementer96 is 96,
    Door96 is 2811231073860124545 ;
    Line287 = "Door 96 OpenClosed"),
    writeln("Door 96 OpenClosed"),
    Current96 is 97,
    Line288 = "Door 97 ",
    (false ->
    Line289 = "Door 97 Open",
    Incrementer97 is 97,
    Door97 is -4011746941211193180 ;
    Line290 = "Door 97 OpenClosed"),
    writeln("Door 97 OpenClosed"),
    Current97 is 98,
    Line291 = "Door 98 ",
    (false ->
    Line292 = "Door 98 Open",
    Incrementer98 is 98,
    Door98 is -5769574690796347507 ;
    Line293 = "Door 98 OpenClosed"),
    writeln("Door 98 OpenClosed"),
    Current98 is 99,
    Line294 = "Door 99 ",
    (false ->
    Line295 = "Door 99 Open",
    Incrementer99 is 99,
    Door99 is 661171896157697102 ;
    Line296 = "Door 99 OpenClosed"),
    writeln("Door 99 OpenClosed"),
    Current99 is 100,
    Line297 = "Door 100 ",
    (false ->
    Line298 = "Door 100 Open",
    Incrementer100 is 100,
    Door100 is -7669786679068496063 ;
    Line299 = "Door 100 OpenClosed"),
    writeln("Door 100 OpenClosed").
