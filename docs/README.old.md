# tersmu 0.1rc1 (historical)

> This is the historical README from an earlier release. See the project root [README](../README) for current documentation.

## Requirements

**Compile-time:**

- GHC
- Pappy — the Makefile automatically wgets, patches and compiles this.

**Run-time:**

- `vlatai`, part of jbofihe, should be in your path. You can get it at:  
  www.rpcurnow.force9.co.uk/jbofihe/index.html

## Description

tersmu is a semantic parser for a fragment of the engineered human language Lojban (www.lojban.org). It translates Lojban to (a mild extension of) first-order logic. The intention is that it be (at least eventually) useful for the purposes of language learning, for increasing the precision of the specification of the language, and in lojban-using computer programs. Currently, tersmu handles a rather restricted part of lojban; in particular, it does not handle tenses or modals, nor complicated anaphora, nor UI, and it does not handle description sumti properly.

### la tersmu goi ty zo'u tu'e (Lojban self-description)

```
da poi pagbu be lopa mulno ke lojbo gerna zo'u
    ty te smuni ro se cusku poi te gerna la lojban da ku'o pa smuni be
    su'o se cusku pe su'o milxe se pagbu be la pamoi te galtu logji
.i pacna pa du'u ty balvi ju cabna se pilno su'o nu cilre gi'a jimpe gi'a
satci zmadu gasnu le ve skicu be la lojban gi'a lojbo samru'e
.i se cabna lo nu ty kakne co te smuni no te gerna be fi su'o da poi de
se cmavo ke'a e ke zo pu a zo bai a zo ui ku'o gi'e nai xamgu te smuni
su'o de poi ga pluja zbasu ke'a su'o cmavo be le selma'o be zo ko'a gi
ke'a se gadri
```

## Sample output

Here's what it makes of the previous paragraph (the indentation was done by hand… automated pretty-printing is on TODO!)

**Prop:**

```
{la} x1:(tersmu(_)). (
    EX x2:({lo} x3:((EQ(1) mei(_) /\ <mulno(_)><<lojbo(_)><gerna>>(_))).
	    pagbu(_,x3)).
	FA x3:((cusku( ,_) /\ gerna(x2,lojban,_))).
	    EQ(1) x4:(
		    EX x5:({la} x6:(<<EQ(1) moi(_)><galtu>( , ,_)><logji>(_)).
			    <milxe(_)><pagbu>(x6,_)).
			EX x6:((cusku( ,_) /\ srana(x5,_))). smuni(_,x6)).
		smuni(x4,x3,x1)
    /\ (EQ(1) x2:(du'u[EX x3:(nu[{le} x4:(skicu( ,lojban, ,_)).
		    (((cilre() \/ jimpe()) \/
			    <<satci(_)><zmadu>(_)><gasnu>( ,x4))
		    \/ <lojbo(_)><samru'e>())](_)).
		<balvi(_)><pilno>(x3,x1)](_)).
	    pacna( ,x2)
    /\ {lo} x2:(nu[(<EQ(0) x3:(
		    EX x4:(EX x5. (cmavo(_,x5) /\ ((cmavo({pu},x5,_) \/
			    cmavo({bai},x5,_)) \/ cmavo({ui},x5,_)))).
			gerna(x4, ,_)).
		smuni( ,x3,_)><kakne>(x1)
		/\ !EX x3:((EX x4:({le} x5:(selma'o(_,{ko'a})). cmavo(_,x5)).
			<pluja(_)><zbasu>( ,_,x4) \/
			gadri( ,_))). <xamgu(_)><smuni>( ,x3,x1))](_)).
	    cabna(x2)))
```

**jbo:** (round-trip output)

```
la tersmu ku goi ko'a zo'u ge su'o da poi lo poi'i ge ke'a 1 mei gi ke'a
ke mulno ke lojbo gerna ke'e ke'e kei ku goi ko'e zo'u ke'a pagbu ko'e ku'o ro
de poi ge zo'e cusku ke'a gi da gerna la lojban. ke'a ku'o 1 di poi su'o da xi
vo poi la ke te ke 1 moi galtu ke'e logji ke'e ku goi ko'e zo'u ko'e ke milxe
pagbu ke'e ke'a ku'o su'o da xi mu poi ge zo'e cusku ke'a gi da xi vo srana
ke'a ku'o zo'u ke'a smuni da xi mu ku'o zo'u di smuni de ko'a gi ge 1 da poi
ke'a du'u su'o de poi ke'a nu ga ga ga cilre gi jimpe gi le ve skicu be la
lojban. ku goi ko'e zo'u zo'e ke ke satci zmadu ke'e gasnu ke'e ko'e gi ke
lojbo samru'e ke'e kei ku'o zo'u de ke balvi pilno ke'e ko'a kei ku'o zo'u
zo'e pacna da gi lo nu ge ko'a ke poi'i 0 da poi su'o de poi su'o di zo'u ge
ke'a cmavo di gi ga ga zo pu cmavo di ke'a gi zo bai cmavo di ke'a gi zo ui
cmavo di ke'a ku'o zo'u de gerna zo'e ke'a ku'o zo'u zo'e smuni da ke'a kei
kakne ke'e gi na ku su'o da poi ga su'o de poi le selma'o be zo ko'a ku goi
ko'i zo'u ke'a cmavo ko'i ku'o zo'u zo'e ke pluja zbasu ke'e ke'a de gi zo'e
gadri ke'a ku'o zo'u zo'e ke xamgu smuni ke'e da ko'a kei ku goi ko'e zo'u
ko'e cabna
```
