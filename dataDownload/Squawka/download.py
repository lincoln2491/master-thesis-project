__author__ = 'ksurdyk'

import urllib
from lxml import etree

spanishLeagueId = "23"
englishLeagueId = "8"
seasons = [2012, 2013, 2014]
pages = range(1, 14)


def getLeagueUrl(id, season, page):
    result = "http://www.squawka.com/match-results?ctl=" + id + "_s" + season.__str__() + "&pg=" + page.__str__()
    return result


def getHtmlFromUrl(url):
    sock = urllib.urlopen(url)
    htmlSource = sock.read()
    sock.close()
    return htmlSource

def getAllLinksFromPage(page):
    root = etree.fromstring(page)
    
tmp = getLeagueUrl(spanishLeagueId, "2013", 12)
tmp = getHtmlFromUrl(tmp)



print tmp
