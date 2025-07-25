// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

std::vector<std::string> fields(std::string s) {
  std::vector<std::string> words = std::vector<>{};
  auto cur = std::string("");
  auto i = 0;
  while ((i < s.size())) {
    auto ch = std::string(s).substr(i, ((i + 1)) - (i));
    if ((((ch == std::string(" ")) || (ch == std::string("\n"))) ||
         (ch == std::string("\t")))) {
      if ((cur.size() > 0)) {
        words.push_back(cur);
        cur = std::string("");
      }
    } else {
      cur = (cur + ch);
    }
    i = (i + 1);
  }
  if ((cur.size() > 0)) {
    words.push_back(cur);
  }
  return words;
}

std::string takeRunes(std::string s, int n) {
  auto idx = 0;
  auto count = 0;
  while ((idx < s.size())) {
    if ((count == n)) {
      return std::string(s).substr(0, (idx) - (0));
    }
    idx = (idx + 1);
    count = (count + 1);
  }
  return s;
}

std::vector<std::string> distinct(std::vector<std::string> xs) {
  auto m = std::unordered_map<int, int>{};
  std::vector<std::string> out = std::vector<>{};
  auto i = 0;
  while ((i < xs.size())) {
    std::vector<std::string> x = xs[i];
    if ((!((m.count(x) > 0)))) {
      m[x] = true;
      out.push_back(x);
    }
    i = (i + 1);
  }
  return out;
}

int abbrevLen(std::vector<std::string> words) {
  auto size = words.size();
  auto l = 1;
  while (true) {
    std::vector<std::string> abbrs = std::vector<>{};
    auto i = 0;
    while ((i < size)) {
      abbrs.push_back(takeRunes(words[i], l));
      i = (i + 1);
    }
    if ((distinct(abbrs).size() == size)) {
      return l;
    }
    l = (l + 1);
  }
  return 0;
}

std::string pad2(int n) {
  auto s = std::to_string(n);
  if ((s.size() < 2)) {
    return (std::string(" ") + s);
  }
  return s;
}

auto __mochi_main() {
  std::vector<std::string> lines = {
      std::string("Sunday Monday Tuesday Wednesday Thursday Friday Saturday"),
      std::string("Sondag Maandag Dinsdag Woensdag Donderdag Vrydag Saterdag"),
      std::string("E_djelë E_hënë E_martë E_mërkurë E_enjte E_premte E_shtunë"),
      std::string("Ehud Segno Maksegno Erob Hamus Arbe Kedame"),
      std::string("Al_Ahad Al_Ithinin Al_Tholatha'a Al_Arbia'a Al_Kamis "
                  "Al_Gomia'a Al_Sabit"),
      std::string("Guiragui Yergou_shapti Yerek_shapti Tchorek_shapti "
                  "Hink_shapti Ourpat Shapat"),
      std::string("domingu llunes martes miércoles xueves vienres sábadu"),
      std::string("Bazar_gÜnÜ Birinci_gÜn Çkinci_gÜn ÜçÜncÜ_gÜn DÖrdÜncÜ_gÜn "
                  "Bes,inci_gÜn Altòncò_gÜn"),
      std::string(
          "Igande Astelehen Astearte Asteazken Ostegun Ostiral Larunbat"),
      std::string("Robi_bar Shom_bar Mongal_bar Budhh_bar BRihashpati_bar "
                  "Shukro_bar Shoni_bar"),
      std::string("Nedjelja Ponedeljak Utorak Srijeda Cxetvrtak Petak Subota"),
      std::string("Disul Dilun Dimeurzh Dimerc'her Diriaou Digwener Disadorn"),
      std::string("nedelia ponedelnik vtornik sriada chetvartak petak sabota"),
      std::string("sing_kei_yaht sing_kei_yat sing_kei_yee sing_kei_saam "
                  "sing_kei_sie sing_kei_ng sing_kei_luk"),
      std::string(
          "Diumenge Dilluns Dimarts Dimecres Dijous Divendres Dissabte"),
      std::string("Dzeenkk-eh Dzeehn_kk-ehreh Dzeehn_kk-ehreh_nah_kay_dzeeneh "
                  "Tah_neesee_dzeehn_neh Deehn_ghee_dzee-neh "
                  "Tl-oowey_tts-el_dehlee Dzeentt-ahzee"),
      std::string(
          "dy_Sul dy_Lun dy_Meurth dy_Mergher dy_You dy_Gwener dy_Sadorn"),
      std::string("Dimanch Lendi Madi Mèkredi Jedi Vandredi Samdi"),
      std::string("nedjelja ponedjeljak utorak srijeda cxetvrtak petak subota"),
      std::string("nede^le ponde^lí úterÿ str^eda c^tvrtek pátek sobota"),
      std::string(
          "Sondee Mondee Tiisiday Walansedee TOOsedee Feraadee Satadee"),
      std::string("s0ndag mandag tirsdag onsdag torsdag fredag l0rdag"),
      std::string("zondag maandag dinsdag woensdag donderdag vrijdag zaterdag"),
      std::string("Diman^co Lundo Mardo Merkredo ^Jaùdo Vendredo Sabato"),
      std::string(
          "pÜhapäev esmaspäev teisipäev kolmapäev neljapäev reede laupäev"),
      std::string("Diu_prima Diu_sequima Diu_tritima Diu_quartima Diu_quintima "
                  "Diu_sextima Diu_sabbata"),
      std::string("sunnudagur mánadagur tÿsdaguy mikudagur hósdagur "
                  "friggjadagur leygardagur"),
      std::string("Yek_Sham'beh Do_Sham'beh Seh_Sham'beh Cha'har_Sham'beh "
                  "Panj_Sham'beh Jom'eh Sham'beh"),
      std::string(
          "sunnuntai maanantai tiistai keskiviiko torsktai perjantai lauantai"),
      std::string("dimanche lundi mardi mercredi jeudi vendredi samedi"),
      std::string("Snein Moandei Tiisdei Woansdei Tonersdei Freed Sneon"),
      std::string("Domingo Segunda_feira Martes Mércores Joves Venres Sábado"),
      std::string("k'vira orshabati samshabati otkhshabati khutshabati "
                  "p'arask'evi shabati"),
      std::string(
          "Sonntag Montag Dienstag Mittwoch Donnerstag Freitag Samstag"),
      std::string(
          "Kiriaki' Defte'ra Tri'ti Teta'rti Pe'mpti Paraskebi' Sa'bato"),
      std::string(
          "ravivaar somvaar mangalvaar budhvaar guruvaar shukravaar shanivaar"),
      std::string("pópule pó`akahi pó`alua pó`akolu pó`ahá pó`alima pó`aono"),
      std::string("Yom_rishon Yom_sheni Yom_shlishi Yom_revi'i Yom_chamishi "
                  "Yom_shishi Shabat"),
      std::string("ravivara somavar mangalavar budhavara brahaspativar "
                  "shukravara shanivar"),
      std::string("vasárnap hétfö kedd szerda csütörtök péntek szombat"),
      std::string("Sunnudagur Mánudagur ╞riδjudagur Miδvikudagar Fimmtudagur "
                  "FÖstudagur Laugardagur"),
      std::string("sundio lundio mardio merkurdio jovdio venerdio saturdio"),
      std::string("Minggu Senin Selasa Rabu Kamis Jumat Sabtu"),
      std::string("Dominica Lunedi Martedi Mercuridi Jovedi Venerdi Sabbato"),
      std::string("Dé_Domhnaigh Dé_Luain Dé_Máirt Dé_Ceadaoin Dé_ardaoin "
                  "Dé_hAoine Dé_Sathairn"),
      std::string("domenica lunedí martedí mercoledí giovedí venerdí sabato"),
      std::string("Nichiyou_bi Getzuyou_bi Kayou_bi Suiyou_bi Mokuyou_bi "
                  "Kin'you_bi Doyou_bi"),
      std::string(
          "Il-yo-il Wol-yo-il Hwa-yo-il Su-yo-il Mok-yo-il Kum-yo-il To-yo-il"),
      std::string("Dies_Dominica Dies_Lunæ Dies_Martis Dies_Mercurii "
                  "Dies_Iovis Dies_Veneris Dies_Saturni"),
      std::string(
          "sve-tdien pirmdien otrdien tresvdien ceturtdien piektdien sestdien"),
      std::string("Sekmadienis Pirmadienis Antradienis Trec^iadienis "
                  "Ketvirtadienis Penktadienis S^es^tadienis"),
      std::string("Wangu Kazooba Walumbe Mukasa Kiwanuka Nnagawonye Wamunyi"),
      std::string("xing-_qi-_rì xing-_qi-_yi-. xing-_qi-_èr xing-_qi-_san-. "
                  "xing-_qi-_sì xing-_qi-_wuv. xing-_qi-_liù"),
      std::string("Jedoonee Jelune Jemayrt Jecrean Jardaim Jeheiney Jesam"),
      std::string("Jabot Manre Juje Wonje Taije Balaire Jarere"),
      std::string(
          "geminrongo minòmishi mártes mièrkoles misheushi bèrnashi mishábaro"),
      std::string("Ahad Isnin Selasa Rabu Khamis Jumaat Sabtu"),
      std::string("sφndag mandag tirsdag onsdag torsdag fredag lφrdag"),
      std::string("lo_dimenge lo_diluns lo_dimarç lo_dimèrcres lo_dijòus "
                  "lo_divendres lo_dissabte"),
      std::string(
          "djadomingo djaluna djamars djarason djaweps djabièrna djasabra"),
      std::string(
          "Niedziela Poniedzial/ek Wtorek S,roda Czwartek Pia,tek Sobota"),
      std::string("Domingo segunda-feire terça-feire quarta-feire quinta-feire "
                  "sexta-feira såbado"),
      std::string("Domingo Lunes martes Miercoles Jueves Viernes Sabado"),
      std::string("Duminicª Luni Mart'i Miercuri Joi Vineri Sâmbªtª"),
      std::string(
          "voskresenie ponedelnik vtornik sreda chetverg pyatnitsa subbota"),
      std::string("Sunday Di-luain Di-màirt Di-ciadain Di-ardaoin Di-haoine "
                  "Di-sathurne"),
      std::string("nedjelja ponedjeljak utorak sreda cxetvrtak petak subota"),
      std::string(
          "Sontaha Mmantaha Labobedi Laboraro Labone Labohlano Moqebelo"),
      std::string("Iridha- Sandhudha- Anga.haruwa-dha- Badha-dha- "
                  "Brahaspa.thindha- Sikura-dha- Sena.sura-dha-"),
      std::string("nedel^a pondelok utorok streda s^tvrtok piatok sobota"),
      std::string("Nedelja Ponedeljek Torek Sreda Cxetrtek Petek Sobota"),
      std::string("domingo lunes martes miércoles jueves viernes sábado"),
      std::string(
          "sonde mundey tude-wroko dride-wroko fode-wroko freyda Saturday"),
      std::string(
          "Jumapili Jumatatu Jumanne Jumatano Alhamisi Ijumaa Jumamosi"),
      std::string("söndag måndag tisdag onsdag torsdag fredag lordag"),
      std::string("Linggo Lunes Martes Miyerkoles Huwebes Biyernes Sabado"),
      std::string("Lé-pài-jít Pài-it Pài-jï Pài-sañ Pài-sì Pài-gÖ. Pài-lák"),
      std::string("wan-ar-tit wan-tjan wan-ang-kaan wan-phoet "
                  "wan-pha-ru-hat-sa-boh-die wan-sook wan-sao"),
      std::string(
          "Tshipi Mosupologo Labobedi Laboraro Labone Labotlhano Matlhatso"),
      std::string("Pazar Pazartesi Sali Çar,samba Per,sembe Cuma Cumartesi"),
      std::string(
          "nedilya ponedilok vivtorok sereda chetver pyatnytsya subota"),
      std::string(
          "Chu?_Nhâ.t Thú*_Hai Thú*_Ba Thú*_Tu* Thú*_Na'm Thú*_Sáu Thú*_Ba?y"),
      std::string("dydd_Sul dyds_Llun dydd_Mawrth dyds_Mercher dydd_Iau "
                  "dydd_Gwener dyds_Sadwrn"),
      std::string("Dibeer Altine Talaata Allarba Al_xebes Aljuma Gaaw"),
      std::string("iCawa uMvulo uLwesibini uLwesithathu uLuwesine uLwesihlanu "
                  "uMgqibelo"),
      std::string("zuntik montik dinstik mitvokh donershtik fraytik shabes"),
      std::string("iSonto uMsombuluko uLwesibili uLwesithathu uLwesine "
                  "uLwesihlanu uMgqibelo"),
      std::string("Dies_Dominica Dies_Lunæ Dies_Martis Dies_Mercurii "
                  "Dies_Iovis Dies_Veneris Dies_Saturni"),
      std::string("Bazar_gÜnÜ Bazar_ærtæsi Çærs,ænbæ_axs,amò Çærs,ænbæ_gÜnÜ "
                  "CÜmæ_axs,amò CÜmæ_gÜnÜ CÜmæ_Senbæ"),
      std::string("Sun Moon Mars Mercury Jove Venus Saturn"),
      std::string("zondag maandag dinsdag woensdag donderdag vrijdag zaterdag"),
      std::string("KoseEraa GyoOraa BenEraa Kuoraa YOwaaraa FeEraa Memenaa"),
      std::string(
          "Sonntag Montag Dienstag Mittwoch Donnerstag Freitag Sonnabend"),
      std::string("Domingo Luns Terza_feira Corta_feira Xoves Venres Sábado"),
      std::string("Dies_Solis Dies_Lunae Dies_Martis Dies_Mercurii Dies_Iovis "
                  "Dies_Veneris Dies_Sabbatum"),
      std::string("xing-_qi-_tiàn xing-_qi-_yi-. xing-_qi-_èr xing-_qi-_san-. "
                  "xing-_qi-_sì xing-_qi-_wuv. xing-_qi-_liù"),
      std::string(
          "djadomingu djaluna djamars djarason djaweps djabièrnè djasabra"),
      std::string("Killachau Atichau Quoyllurchau Illapachau Chaskachau "
                  "Kuychichau Intichau")};
  auto i = 0;
  while ((i < lines.size())) {
    auto words = fields(lines[i]);
    auto l = abbrevLen(words);
    std::cout << ((pad2(l) + std::string("  ")) + lines[i]) << std::endl;
    i = (i + 1);
  }
}

int main() {
  __mochi_main();
  return 0;
}
