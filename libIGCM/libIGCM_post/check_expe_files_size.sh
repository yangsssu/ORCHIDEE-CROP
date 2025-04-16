#!/bin/sh
##set -xv
#**************************************************************
# Author: Marie-Alice Foujols
# Contact: Marie-Alice.Foujols__at__ipsl.jussieu.fr
# $Revision:: 1059                                     $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2014-09-23 12:05:29 +0200 (Tue, 23 Sep 2014) $ Date of last commit
# IPSL (2009)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************
# stephane.senesi 'at' meteo.fr, CNRM-GAME/GMGEC, mars-avril 2010
doc="\nScript d'analyse des fichiers d'un repertoire pour y trouver des formes \n\
de nom de fichier avec dates, les dates, et des sequences de dates. \n\
On synthetise la
\n\n\t Syntaxe : $(basename $0) [-d] [-v/q] [-R] [-Z] dir [-S subdir] [-I maxyear]\n\n\
L'option -R permet d'inclure les sous-repertoires\n\
L'option -Z permet de verifier que les tailles des fichiers mensuels sont identiques par mois\n\
dir permet de decrire le repertoire selectionne : . par defaut \n\
-S subdir permet de decrire les sous-repertoires selectionnes : \*/Output par exemple\n\
-I maxyear ne traite que les dates inferieures ou egales a maxyear. Toutes les dates par defaut\n\
Les formes de dates identifiables sont illustrees dans l'exemple obtenu par \n\
    \t$(basename $0) -exemple\n\
la valeur retournee est le nombre de formes de noms de fichier qui presentent des trous\n
Si l option -Z est precisee, le code retour y ajoute le nombre de mois avec tailles differentes.\n
-q pour n avoir aucun message / -v pour avoir les patterns et erreurs  a  l ecran \n
-d pour activer l echo des commandes \n
Exemples :\n
check_expe_files_size.sh SRF/Output/MO # verification des noms du repertoire SRF/Output/MO\n
check_expe_files_size.sh -Z SRF/Output/MO # verification des noms et des tailles des fichiers du repertoire\n
check_expe_files_size.sh -R -S \"*/Output\" # verification des noms des fichiers pour toute une simulation IPSL\n
check_expe_files_size.sh -Z -R -S \"*/Output\" # verification des noms et des tailles des fichiers pour toute une simulation IPSL\n
check_expe_files_size.sh -I 1949 -R -S \"*/Output\" # verification jusuque a l annee 1949 incluse des noms des fichiers pour une simulation IPSL\n
"
#La presence d'une annee+mois impose la presence de l'annee entiere
[ $# -eq 0  ]  && echo -e $doc && exit -1

debugflag=0
quiet=0
exemple=0
recursion=0
examsize=0
dir=.
subdir=
maxyear=9999
maxyearp1=9999

#---------------------------------------------
while [ $# -ne 0 ]
do
  case $1 in
  -h|--help|-help)
    echo -e $doc
    exit -1 ;;
  -e|-exemple)
    exemple=1
# exemple de formes de dates reconnues par ce script. La liste ci-dessous est aussi
# utilisee lors des tests du script
    cat<<-EOF > /tmp/$$.ldir
MONEXP_20000101_20000131_1M_histmth.nc
MONEXP_20000101_20000131_1M_histmthNMC.nc
MONEXP_20000201_20000228_1M_histmth.nc
MONEXP_20000201_20000228_1M_histmthNMC.nc
MONEXP_20000301_20000331_1M_histmth.nc
MONEXP_20000301_20000331_1M_histmthNMC.nc
MONEXP_20000401_20000430_1M_histmth.nc
MONEXP_20000401_20000430_1M_histmthNMC.nc
MONEXP_20000501_20000531_1M_histmth.nc
MONEXP_20000501_20000531_1M_histmthNMC.nc
MONEXP_20000601_20000630_1M_histmth.nc
MONEXP_20000601_20000630_1M_histmthNMC.nc
MONEXP_20000701_20000731_1M_histmth.nc
MONEXP_20000701_20000731_1M_histmthNMC.nc
MONEXP_20000801_20000831_1M_histmth.nc
MONEXP_20000801_20000831_1M_histmthNMC.nc
MONEXP_20000901_20000930_1M_histmth.nc
MONEXP_20000901_20000930_1M_histmthNMC.nc
MONEXP_20001001_20001031_1M_histmth.nc
MONEXP_20001001_20001031_1M_histmthNMC.nc
MONEXP_20001101_20001130_1M_histmthNMC.nc
MONEXP_20001201_20001231_1M_histmth.nc
MONEXP_20001201_20001231_1M_histmthNMC.nc

EOF
    echo -e "\nExample : with this directory content\n"
    cat  /tmp/$$.ldir
    echo -e "\n\nYou would get :\n\n"
    break;;
  -d|--debug)
    set -x
    debugflag=1
    shift ;;
  -q|--quiet)
    quiet=1
    shift ;;
  -v|--verbose)
    quiet=0
    shift ;;
  -R|-r|--r)
    recursion=1
    shift ;;
  -Z|-z|--z)
    examsize=1
    shift ;;
  -S|-s|--s)
    subdir=$2
    shift 2 ;;
  -I|-i|--i)
    maxyear=$2
    (( maxyearp1 = maxyear + 1 ))
    shift 2 ;;
  *)
    dir=$1
    break ;;
  esac
done

[ $quiet = 0 ] && echo quiet : $quiet recursion : $recursion - examsize : $examsize - dir - $dir -- subdir $subdir-- maxyearp1 : $maxyearp1
if [ $exemple = 0 ] ; then
  [ ! -d $dir ] && echo "verif_expe_files : missing directory $dir "&& exit
  ( cd $dir
    if [ $recursion = O ] ; then ls > /tmp/$$.ldir ; [ $examsize = 1 ] && ls -l > /tmp/$$.size # Cas "normal" : on liste les entrees du repertoire et ls long
    else find ./$subdir  -type f > /tmp/$$.ldir ; [ $examsize = 1 ] && find ./$subdir -type f -exec ls -l {} \; >/tmp/$$.size # Cas de listage recursif des fichiers
    fi
  )
fi
###head /tmp/$$.ldir ; head /tmp/$$.size ; rm /tmp/$$*
# For debugging purpose ...
#head -n 2 /tmp/$$.ldir > /tmp/$$.aa ; mv /tmp/$$.aa /tmp/$$.ldir
#set -x
# On remplace les / en ~ dans la liste des fichiers
cat /tmp/$$.ldir | tr "/" "~" > /tmp/$$.aa ; mv /tmp/$$.aa /tmp/$$.ldir
[ -f /tmp/$$.size ] && cat /tmp/$$.size | tr "/" "~" > /tmp/$$.aa && mv /tmp/$$.aa /tmp/$$.size
# On deduit de la liste des fichiers des patterns de annee+mois, annee, intervalles de date ...
sed -r \
  -e 's/[0-5][0-9]{3}[01][0-9]/YYMM/g' \
  -e 's/YYMM01_YYMM(28|29|30|31)/YYmmdd_YYmmdd/g' \
  /tmp/$$.ldir | sort -u > /tmp/$$.lpat
# Ci-dessus, il faudrait compter les nb d'occ de chqaue pattern pour signaler et
# ne pas retenir ceux qui n'apparaissent qu'une fois (sauf les decennaux ?)
# On ote les patterns sans date du tout
sed -i -e '/YY/!d' /tmp/$$.lpat
# On rend compte des patterns identifies
#echo "Formes de fichier identifiees: " ; awk '{printf "\t%s\n",$1}' /tmp/$$.lpat
# Une fonction pour associer un nom de fichier a chaque pattern : on remplace les / par des ~
tmpfic (){
  echo -n "/tmp/$$." ; echo $1 | tr "/" "_"
}
# Pour chaque pattern, on liste les dates pertinentes: annees de debut et fin pour les
# intervalles d'annees, ou annee ou annee+mois pour les autres
cat /dev/null > /tmp/$$.dates
#set -x
while read pattern ; do
  if [ $(echo $pattern | grep "YYmmdd_YYmmdd") ] ; then
        # On transforme le pattern pour mettre une sequences
        # de capture des annees de debut et fin d'intervalle
    p2=$(echo $pattern | sed -e 's/YYmmdd_YYmmdd/([0-5][0-9]{3}[01][0-9])01_[0-5][0-9]{3}[01][0-9](28|29|30|31)/g' )

       # On balaye le repertoire avec le(s) pattern(s) transforme(s) pour capturer les annees
    grep -E $p2 /tmp/$$.ldir | sed -r -e "s/$p2/\1/g"  >> /tmp/$$.$pattern.tmp
  fi
  sort -n -u /tmp/$$.$pattern.tmp > /tmp/$$.$pattern.list
    # Si la liste des date (annees ou annes+mois) pour le pattern est
    # de taille > 1, on l'integre a la liste generale des dates
    # en supprimant au prealable les dates isolees
    # ... ca reste a faire
  cat /tmp/$$.$pattern.list | cut -c 1-4 >> /tmp/$$.dates
done < /tmp/$$.lpat
# Fonction pour completer et rendre sequentielle une liste d'annees (une par ligne)
force_sequence (){
  awk '{
        if (NR==1) {avant=$1; print $1}
        else { for (i=avant+1 ; i<=$1 ; i++) printf "%04i\n", i}}'
}
sort -u /tmp/$$.dates > /tmp/$$.toutes_dates
# On enleve les trous dans la liste des dates
force_sequence < /tmp/$$.toutes_dates > /tmp/$$.tmp ; sort -u /tmp/$$.tmp > /tmp/$$.toutes_dates
# Fonction d'analyse de sequences de nombres (annees, ou annees+mois
# On transforme les series continues a a+1 a+2....b (a raison d'un par
# ligne) en : "a-b"
sequence (){
  awk -v pas=${1:-1} '{
        if (NR==1) {avant=$1;nb=1; printf "%s",$1}
        else {
          suivant=avant+pas
          # Pour les nombres sur 6 chiffres, on suppose que c est du YYYMM
          # et on gere le passage de decembre a janvier
          if ((length(avant)==6) && (substr(avant,5,2)=="12"))
            { avant4=substr(avant,1,4) ; suivant=sprintf("%4d01",avant4+1)}
          if ($1 > suivant) {
            if (nb>1) printf "-%s",avant
            nb=1 ; printf ", %s",$1}
          else { nb=nb+1 }
          avant=$1  }}
      END { if (nb >1) printf "-%s\n",avant ; else print ""} '
}
[ $quiet = 0 ] && echo $(wc -l /tmp/$$.ldir | cut -d \  -f 1) files proceeded.
[ $quiet = 0 ] && echo -n "Years occurring : " && sequence 1 < /tmp/$$.toutes_dates
# on ne garde que les annees inferieures a maxyear
sed "/$maxyearp1/"',$d' </tmp/$$.toutes_dates >/tmp/$$.toutes_dates_maxyear
[ $quiet = 0 ] && echo -n "Years tested : " && sequence 1 < /tmp/$$.toutes_dates_maxyear
mv /tmp/$$.toutes_dates_maxyear /tmp/$$.toutes_dates
# On liste tous les mois qui doivent etre presents (au motif qu'une annee est presente)
> /tmp/$$.toutes_dates_et_mois
while read an ; do
  i=1
  while [ $i -le 12 ] ; do
    printf "%s%02d\n" ${an} $i  >> /tmp/$$.toutes_dates_et_mois
    i=$(( i + 1 ))
  done
done < /tmp/$$.toutes_dates
# Pour chaque pattern, on analyse si toutes les dates sont presentes en bon nombre (decennie, an, an+mois)
touch /tmp/$$.synthese
[ $quiet = 0 ] && echo "File name patterns and holes (if any) :"
#set -x
while read pattern ; do
  [ `echo $pattern | grep -E "(YYmmdd_YYmmdd)"` ]  && pas=1 && \
    join -v 1 /tmp/$$.toutes_dates_et_mois  /tmp/$$.$pattern.list > /tmp/$$.$pattern.manques
  sequence $pas < /tmp/$$.$pattern.manques > /tmp/$$.$pattern.synthese
  truepattern=$(echo $pattern | tr "~" "/")
  [ $quiet = 0 ] && printf "%80s : " $truepattern  && cat /tmp/$$.$pattern.synthese
  cat /tmp/$$.$pattern.synthese >> /tmp/$$.synthese
done < /tmp/$$.lpat

rep=$(wc -w /tmp/$$.synthese | tail -1 | cut -d \  -f 1)
if [ $examsize = 0 ] ; then rm /tmp/$$* ; exit $rep ; fi

# taille des fichiers
cat /dev/null > /tmp/$$.dates
#set -x
while read pattern ; do
  i=1
  while [ $i -le 12 ] ; do
    mois=$(printf "%02d\n"  $i)
    if [ $(echo $pattern | grep "YYmmdd_YYmmdd") ] ; then
           # On transforme le pattern pour mettre une sequences
           # avec le mois courant
      px=$(echo $pattern | sed -e "s/YYmmdd_YYmmdd/([0-5][0-9]{3})${mois}01_[0-5][0-9]{3}${mois}(28|29|30|31)/g" )

           # On balaye le repertoire avec le(s) pattern(s) transforme(s) pour capturer les tailles des fichiers de ce mois
      grep -E $px /tmp/$$.size | awk '{print $5}' | sort -u >/tmp/$$.$pattern.size_${mois}
           # On garde les infos s il n y a pas une taille unique
      for size in $(cat /tmp/$$.$pattern.size_${mois})  ; do
        grep -E $px /tmp/$$.size | grep " "${size}" " | tr "~" "/" >>/tmp/$$.$pattern.files_${mois}.${size}
      done
    fi
    i=$(( i + 1 ))
  done
done < /tmp/$$.lpat

[ $quiet = 0 ] && echo
[ $quiet = 0 ] && echo  "Size verification per month (if different) :"
# Pour chaque pattern, on liste la taille des fichiers de chaque mois
touch /tmp/$$.synthese_size
while read pattern ; do
  truepattern=$(echo $pattern | tr "~" "/")
  [ $quiet = 0 ] && printf "%80s : " $truepattern
  if [ `echo $pattern | grep -E "(YYmmdd_YYmmdd)"` ]  ; then
    RESULT=""
    for i in 1 2 3 4 5 6 7 8 9 10 11 12 ; do
      mois=$(printf "%02d\n"  $i)
      if [ $(wc -l /tmp/$$.$pattern.size_${mois} |  cut -d " " -f 1) = 1 ] ; then
        RESULT="$RESULT "
      else
                # special case for february , we accept 2 different sizes for february for leap year
        if [[ $i == 2 && $(wc -l /tmp/$$.$pattern.size_${mois} |  cut -d " " -f 1) = 2 ]] ; then
                   # est ce qu il y a des mois de 29 jours? si non souci reel
          if [ $( grep 0229 /tmp/$$.$pattern.files_02.* 1>/dev/null 2>&1 ) ] ; then
            RESULT="$RESULT \033[1;31m[$mois]\033[m"
            cat  /tmp/$$.$pattern.size_${mois} >> /tmp/$$.synthese_size
          else
            RESULT="$RESULT "
          fi
        else
          RESULT="$RESULT \033[1;31m[$mois]\033[m"
          cat  /tmp/$$.$pattern.size_${mois} >> /tmp/$$.synthese_size
        fi
      fi
    done
    [ $quiet = 0 ] && echo -e $RESULT
  fi
done < /tmp/$$.lpat

# affichage des details
while read pattern ; do
  first=0
  i=1
  truepattern=$(echo $pattern | tr "~" "/")
  while [ $i -le 12 ] ; do
    mois=$(printf "%02d\n"  $i)
    if [ $(echo $pattern | grep "YYmmdd_YYmmdd") ] ; then
           # si on a plus de 2 tailles de fichiers pour ce mois, on liste le nombre de fichiers par taille et les 3 premiers
           # cas special pour fevrier qui accepte 2 tailles differentes si 1 pour les 28 et l autre pour les 29
      if [ $(wc -l /tmp/$$.$pattern.size_${mois} | cut -d " " -f 1) != 1 ] ; then
        if  [[ $i == 2 && $(wc -l /tmp/$$.$pattern.size_${mois} |  cut -d " " -f 1) == 2 ]] ; then
                   # est ce qu il y a des mois de 29 jours? sinon souci reel
          if [ $( grep 0229 /tmp/$$.$pattern.files_02* 1>/dev/null 2>&1 ) ] ; then
            if [[ ${first} == 0 ]] ; then printf "Details %73s : \n" $truepattern ; first=1 ; fi
            echo mois : ${mois}
            for size in $( cat /tmp/$$.$pattern.size_${mois} ) ; do
              head -1 /tmp/$$.$pattern.files_${mois}.${size}
            done
          fi
        else
          if [[ ${first} == 0 ]] ; then printf "Details %73s : \n" $truepattern ; first=1 ; fi
          echo mois : ${mois}
          for size in $( cat /tmp/$$.$pattern.size_${mois} ) ; do
            head -1 /tmp/$$.$pattern.files_${mois}.${size}
          done
        fi
      fi
    fi
    i=$(( i + 1 ))
  done
done < /tmp/$$.lpat


rep=$(wc -w /tmp/$$.synthese /tmp/$$.synthese_size | tail -1 | sed -e 's/total//' -e 's/ //g' )

# reste  a analyser par annee en sus de par pattern, pour presenter le plus clair
# Penser a forunir l'option d'imposer des patterns et des annees en entree
[ $debugflag = 0 ] && rm -f /tmp/$$.* /tmp/$$.ldir
exit $rep
# Changes
# - 20 april 2010 : add handling of pattern year1-year2, which is a generalization of decades
# - janvier 2011 : suppression des caracteres accentues
#                  mise en place pour prefix de type libIGCM / IPSL
#                  ajout de la verification des tailles des fichiers mensuels
#                  ajout des options -d -q/-v -R -S subdir -I maxyear
