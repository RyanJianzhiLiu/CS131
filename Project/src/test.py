import asyncio
import sys

async def broadcast():
    while(True):
        msg = await receive_msg()
        if msg == "STOP": break
        else: print("Broadcast: " + msg)
    
async def receive_msg():
    msg = input("Enter your message: ")
    return msg

async def end_msg(loop):
    print("End of Broadcast. Bye~")
    loop.stop()
    
loop = asyncio.get_event_loop()
loop.run_until_complete(broadcast())

loop.create_task(end_msg(loop))
loop.run_forever()
loop.close()
