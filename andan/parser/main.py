import re
from lxml import html
import requests
import pandas as pd
import datetime
import locale


locale.setlocale(locale.LC_TIME, 'ru_RU.UTF-8')


def get_flat_links(filename):
    flats = []
    page_format = 'https://spb.cian.ru/cat.php?deal_type=rent&engine_version=2&offer_type=flat&p={0}&region=2&type=4'
    page_count = 1
    while True:
        resp = requests.get(page_format.format(page_count))
        lst = resp.url.split('&p=')
        if not lst[1].startswith(str(page_count) + '&'):
            break
        content = html.fromstring(resp.content)
        flats.extend(content.xpath('//article[@data-name="CardComponent"]/div[@data-testid="offer-card"]/a/@href'))
        page_count += 1
    with open(filename, 'w') as out:
        out.write('\n'.join(flats))


def parse_flat_link(link):
    content = html.fromstring(requests.get(link).content)
    flat_id = link.split('/')[-2]
    result = dict()
    result['ID'] = flat_id

    def get_category(name, xp, func, default=''):
        found = content.xpath(xp.format(name))
        result[name] = [default] if len(found) == 0 else [func(found[0])]

    def convert_price(price):
        s = re.search('[0-9]+', price.replace('\xa0', '').replace(' ', ''))
        return s.group() if s else ''

    def convert_district(addr):
        lst = addr.split()
        try:
            return lst[lst.index('р-н')+1][:-1]
        except ValueError:
            return ''

    def convert_date(date):
        date = date.replace('Обновлено: ', '')
        today = datetime.datetime.today()
        yesterday = today-datetime.timedelta(days=1)
        date = date.replace('сегодня,', today.strftime('%d %m')).replace('вчера,', yesterday.strftime('%d %m'))
        months = ['янв,', 'фев,', 'мар,', 'апр,', 'мая,', 'июня,', 'июля,', 'авг,', 'сен,', 'окт,', 'нояб,', 'дек,']
        for i, m in enumerate(months):
            date = date.replace(m, ('0' if i < 10 else '') + str(i))
        return date

    get_category('Дата', '/html/body/div[2]/div[2]/div[1]/div[2]/div[1]/div[1]/span/text()', convert_date)
    get_category('Цена', '/html/body/div[2]/div[2]/div[3]/div/div[1]/div[1]/div[3]/div/div[1]/span/text()',
                 convert_price)
    get_category('Район', '//div[@data-name="Geo"]/span/@content', convert_district)
    top_left = '//span[contains(text(), "{0}")]/following-sibling::span/text()'
    get_category('Комиссия', top_left, lambda x: float(x.strip('%'))/100, '0')
    get_category('Залог', top_left, convert_price, '0')
    for n in ['Предоплата', 'Срок аренды']:
        get_category(n, '//span[contains(text(), "{0}")]/following-sibling::span/text()', lambda x: x)
    for n in ['Общая площадь', 'Год постройки']:
        get_category(n, '//div[@data-name="CardSectionNew"]//span[contains(text(), '
                        '"{0}")]/following-sibling::span/text()', lambda x: x.replace(',', '.'))
    return result


def main():
    flat_links_path = 'parser/flat_links.txt'
    with open(flat_links_path, 'r') as flat_links_file:
        links = flat_links_file.readlines()
        if len(links) == 0:
            get_flat_links(flat_links_path)
            links = flat_links_file.readlines()
    df = pd.DataFrame(columns=['ID', 'Дата', 'Цена', 'Район', 'Залог', 'Комиссия', 'Предоплата',
                               'Срок аренды', 'Общая площадь', 'Год постройки'])
    for link in links:
        d = parse_flat_link(link.strip())
        df = pd.concat([df, pd.DataFrame(d)], ignore_index=True)
    df.to_csv('parser/table.csv')


main()


