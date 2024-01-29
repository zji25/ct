import requests
from lxml import html
import os
import urllib.request


def get_images(link, dir_name):
    page_count = 1
    images = []
    while True:
        resp = requests.get(link.format(page_count))
        content = html.fromstring(resp.content)
        found = [str(image) for image in content.xpath('//img/@src') if str(image).find('/cdn/shop/products') != -1]
        if len(found) == 0:
            break
        images.extend(found)
        print(f'parsed {page_count} pages')
        if len(images) > 700:
            break
        page_count += 1
    print(images)
    if not os.path.exists(dir_name):
        os.makedirs(dir_name)
        for i, url in enumerate(images):
            try:
                urllib.request.urlretrieve(f'https:{url}', f'{dir_name}/image_{i + 1}.jpg')
                print(f'downloaded image {i + 1}/{len(images)}')
            except Exception as e:
                print(f'an error while downloading image {i + 1}: {str(e)}')


get_images('https://kixbox.ru/collections/mens-clothing?page={0}', 'kixbox_images/clothing')
get_images('https://kixbox.ru/collections/mens-footwear?page={0}', 'kixbox_images/shoes')
get_images('https://kixbox.ru/collections/mens-accessories?page={0}', 'kixbox_images/accessories')

