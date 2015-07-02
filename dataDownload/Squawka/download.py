__author__ = 'ksurdyk'

import urllib2
from bs4 import BeautifulSoup

spanishLeagueId = "23"
englishLeagueId = "8"
seasons = [2012, 2013, 2014]
pages = range(1, 14)


def getLeagueUrl(id, season, page):
    result = "http://www.squawka.com/match-results?ctl=" + id + "_s" + season.__str__() + "&pg=" + page.__str__()
    return result


def getHtmlFromUrl(url):
    req = urllib2.Request(url)
    req.add_header("User-agent", "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:38.0) Gecko/20100101 Firefox/38.0")
    res = urllib2.urlopen(req)
    htmlSource = res.read()
    res.close()
    return htmlSource


def getAllLinksFromPage(page):
    soup = BeautifulSoup(page)
    td = soup.find_all("td", class_="match-centre")
    return td

def getAllLinksForSeason(season, league):
    result = []
    for i in pages:
        url = getLeagueUrl(league, season, i)
        html = getHtmlFromUrl(url)
        print ">>>>>>>>>>>>>"
        print html
        print "<<<<<<<<<<<<<"
        # links = getAllLinksFromPage(html)
        # result. extend(links)
    return result


tmp = getAllLinksForSeason(2012, spanishLeagueId)
# print tmp.__len__()
# print tmp


