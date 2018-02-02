from Bio import Entrez

Entrez.email = "mcfrank@stanford.edu"

def get_links_term(term):
	links = Entrez.esearch(db="pubmed", retmax = 5000, term=term)
	record = Entrez.read(links)
	link_list = record[u'IdList']
	return link_list

def fetch_details(id_list):
    ids = ','.join(id_list)
    handle = Entrez.efetch(db='pubmed',
                           retmode='xml',
                           id=ids)
    results = Entrez.read(handle)
    return results

### MAIN -----------------------

psci_links = get_links_term("\"Psychological science\"[Journal]")

# print(len(psci_links))

papers = fetch_details(psci_links)

articles = [None] * len(psci_links)
years = [None] * len(psci_links)

for i in range(0,len(papers['PubmedArticle'])):
    print(i)
    if len(papers['PubmedArticle'][i]['MedlineCitation']['Article']['ArticleDate']) > 0:
        years[i] =  papers['PubmedArticle'][i]['MedlineCitation']['Article']['ArticleDate'][0]['Year']
    if 'AuthorList' in papers['PubmedArticle'][i]['MedlineCitation']['Article'].keys():
        authors = papers['PubmedArticle'][i]['MedlineCitation']['Article']['AuthorList']
        articles[i] = list()
        for author in authors:
            if ('ForeName' in author.keys()) and ('LastName' in author.keys()):
                articles[i].append([author['ForeName'] + " " + author['LastName']])

print(articles)

f = open('AuthorList.csv', 'w')

f.write("article_id,year,author\n")

for i, article in enumerate(articles):
    if article:
        if years[i]:
            for j, author in enumerate(article):
                f.write(str(i) + "," + years[i] + "," + author[0] + "\n")

f.close()