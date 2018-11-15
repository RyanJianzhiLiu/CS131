import aiohttp
import asyncio
import json

myAPIkey = 'AIzaSyA7lJIidoEdefGNThTmtmGj0Z7d7FJ6lEU'

async def fetch(session, url, lat_lng, r):
    params = {'key':myAPIkey, 'location':lat_lng, 'radius':r}
    async with session.get(url, params=params) as response:
        #print(str(response.url))
        return await response.json()

async def search_resp(lat_lng, r, max_info, transport):
    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'
    async with aiohttp.ClientSession() as session:
        places = await fetch(session, url, lat_lng, r)
    if places['status'] != 'OK':
        if places['status'] != 'ZERO_RESULTS':
            print('Request to Google Place went wrong.')
    else:
        all_results = places['results']
        if len(all_results) >= max_info:
            places['results'] = all_results[0:max_info]
        print('{0}/{1} places sent to client.'.format(len(places['results']),len(all_results)))
    response = json.dumps(places,indent=2)
    transport.write(response.encode())
    transport.write('\r\n\r\n'.encode())
