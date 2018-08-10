from urllib import urlopen, urlretrieve, quote
from urlparse import urljoin
from bs4 import BeautifulSoup

url = 'http://www.cde.ca.gov/ds/sd/sd/filesenr.asp'
u = urlopen(url)
try:
  html = u.read().decode('utf-8')
finally:
  u.close()

soup = BeautifulSoup(html)
for link in soup.find_all('a'):
  href = link.get('href')
  if href.startswith('http:'):
    try:
      res = urllib.urlopen(url)   
      header = res.info() 
      if 'Content-Disposition' in str(header):
        filename = res.info()['Content-Disposition'] . split('=')[-1] . strip('"')
        with open(filename) as code:
           code.write(res)
      #urlretrieve(href, 'filesenr.asp')
    except:
      print('failed to download')
