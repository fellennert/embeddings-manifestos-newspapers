import pandas as pd
import spacy
from spacy.lang.fr.stop_words import STOP_WORDS

STOP_WORDS.add('«')
STOP_WORDS.add('»')
STOP_WORDS.add('·')
STOP_WORDS.add('-')

manifesto = pd.read_csv('./corpus-2002-2017.csv')
mp = pd.read_csv('./mp_france.csv')
manifesto = pd.merge(manifesto, mp[['party','partyname']].drop_duplicates(['party'],keep='last'), on='party', how='left')

lm = pd.read_csv('../Politicking/data/newspapers/LeMonde_20211027.csv')
lf = pd.read_csv('../Politicking/data/newspapers/LeFigaro_20211027.csv')

# corpus manifesto 2002

manifesto2002 = manifesto.loc[[26,28,29,30,31],['partyname','text']].reset_index(drop=True) 
# LV, PS, UDF, RPR, FN
manifesto2002['annee'] = 2002

lm2002 = lm.loc[(lm['annee']>=2001)&(lm['annee']<=2006),['journal','texte','annee']]
lf2002 = lf.loc[(lf['annee']>=2001)&(lf['annee']<=2006),['journal','texte','annee']]

lm2002.columns = ['partyname', 'text', 'annee']
lf2002.columns = ['partyname', 'text', 'annee']

df2002 = pd.concat([manifesto2002,lm2002, lf2002], ignore_index=True)
df2002 = df2002[['text','partyname','annee']]
df2002 = df2002.reset_index()
df2002['fichier'] = 1


# corpus manifesto 2007

manifesto2007 = manifesto.loc[[20,21,22,23,24,25],['partyname','text']].reset_index(drop=True) 
# FN, EELV, MoDem, PCF, PS, UMP
manifesto2007['annee'] = 2007

lm2007 = lm.loc[(lm['annee']>=2006)&(lm['annee']<=2011),['journal','texte','annee']]
lf2007 = lf.loc[(lf['annee']>=2006)&(lf['annee']<=2011),['journal','texte','annee']]

lm2007.columns = ['partyname', 'text', 'annee']
lf2007.columns = ['partyname', 'text', 'annee']

df2007 = pd.concat([manifesto2007,lm2007, lf2007], ignore_index=True)
df2007 = df2007[['text','partyname','annee']]
df2007 = df2007.reset_index()
df2007['fichier'] = 2



# corpus manifesto 2012

manifesto2012 = manifesto.loc[[10,11,13,15,16,19],['partyname','text']].reset_index(drop=True) 
# PG, EELV, PS, MoDem, UMP, FN
manifesto2012['annee'] = 2012

lm2012 = lm.loc[(lm['annee']>=2011)&(lm['annee']<=2016),['journal','texte','annee']]
lf2012 = lf.loc[(lf['annee']>=2011)&(lf['annee']<=2016),['journal','texte','annee']]

lm2012.columns = ['partyname', 'text', 'annee']
lf2012.columns = ['partyname', 'text', 'annee']

df2012 = pd.concat([manifesto2012,lm2012, lf2012], ignore_index=True)
df2012 = df2012[['text','partyname','annee']]
df2012 = df2012.reset_index()
df2012['fichier'] = 3



# corpus manifesto 2017

manifesto2017 = manifesto.loc[[0,3,4,5,8,9],['partyname','text']].reset_index(drop=True) 
# EELV, LFI, PS, EM, LR, FN
manifesto2017['annee'] = 2017

lm2017 = lm.loc[(lm['annee']>=2016),['journal','texte','annee']]
lf2017 = lf.loc[(lf['annee']>=2016),['journal','texte','annee']]

lm2017.columns = ['partyname', 'text', 'annee']
lf2017.columns = ['partyname', 'text', 'annee']

df2017 = pd.concat([manifesto2017,lm2017, lf2017], ignore_index=True)
df2017 = df2017[['text','partyname','annee']]
df2017 = df2017.reset_index()

df2017['fichier'] = 4

df = pd.concat([df2002,df2007,df2012,df2017], ignore_index=True)


nlp = spacy.load("fr_core_news_sm")



docs = nlp.pipe(df.text)

texts = []
for doc in docs:
    words = []
    for tok in doc:
        if tok.pos_ in ['NOUN', 'ADJ']:
            words.append(tok.lemma_)
    texts.append([w for w in words if w not in STOP_WORDS])
    
    
df['words'] = texts

df.to_csv('./manifesto_articles_preprocessed.csv', index=False)


