import binascii
import sys
import argparse




def bitwise_xor_bytes(a, b):
    result_int = int.from_bytes(a, byteorder="big") ^ int.from_bytes(b, byteorder="big")
    return (result_int.to_bytes(max(len(a), len(b)), byteorder="big").decode())


def task1(c1bin,c2bin,guessbin,offset):
    #note all three of c1bin, c2bin,guessbin have been converted into python3 bytes objects
    
    output = bytearray(len(c1bin))
    
    
    if offset == 0: #Insert correct guess into c1bin, turning it into m1 (technically)
            m1 = guessbin
    else:
            m1 = c1bin[0:offset] + guessbin + c1bin[offset + len(guessbin):len(c1bin)]
    
    m1 = m1[:60] #Makes sure length is the same as ciphertext
    
    key = (bytes(a ^ b for (a,b) in zip(c1bin, m1))) #Using XOR property/HW4 spec, get Key. c1 XOR m1 = KEY
    

    m2 = (bytes(a ^ b for (a,b) in zip(c2bin, key))) #Using HW4 Spec, XOR property, and the Key, find m2. c2 XOR Key = m2
    
   
    
    return bytearray(m2) # make sure you return the result for the auto grading

#  $$$$$$$\   $$$$$$\        $$\   $$\  $$$$$$\ $$$$$$$$\       $$\      $$\  $$$$$$\  $$$$$$$\  $$$$$$\ $$$$$$$$\ $$\     $$\
#  $$  __$$\ $$  __$$\       $$$\  $$ |$$  __$$\\__$$  __|      $$$\    $$$ |$$  __$$\ $$  __$$\ \_$$  _|$$  _____|\$$\   $$  |
#  $$ |  $$ |$$ /  $$ |      $$$$\ $$ |$$ /  $$ |  $$ |         $$$$\  $$$$ |$$ /  $$ |$$ |  $$ |  $$ |  $$ |       \$$\ $$  /
#  $$ |  $$ |$$ |  $$ |      $$ $$\$$ |$$ |  $$ |  $$ |         $$\$$\$$ $$ |$$ |  $$ |$$ |  $$ |  $$ |  $$$$$\      \$$$$  /
#  $$ |  $$ |$$ |  $$ |      $$ \$$$$ |$$ |  $$ |  $$ |         $$ \$$$  $$ |$$ |  $$ |$$ |  $$ |  $$ |  $$  __|      \$$  /
#  $$ |  $$ |$$ |  $$ |      $$ |\$$$ |$$ |  $$ |  $$ |         $$ |\$  /$$ |$$ |  $$ |$$ |  $$ |  $$ |  $$ |          $$ |
#  $$$$$$$  | $$$$$$  |      $$ | \$$ | $$$$$$  |  $$ |         $$ | \_/ $$ | $$$$$$  |$$$$$$$  |$$$$$$\ $$ |          $$ |
#  \_______/  \______/       \__|  \__| \______/   \__|         \__|     \__| \______/ \_______/ \______|\__|          \__|
# Helper function to carry out some type conversions
def load_and_format_run(f1,f2,guess,offset):
    ctext1 = f1.readlines()[0]
    ctext2 = f2.readlines()[0]
    c1bin = binascii.unhexlify(ctext1)
    c2bin = binascii.unhexlify(ctext2)
    assert len(c1bin) == len(c2bin) 
    guessbin = guess.encode('ascii')
    output = task1(c1bin,c2bin,guessbin,offset)
    assert isinstance(output,bytearray)==True,"task1(c1bin,c2bin,guessbin,offset) must return a byte array"
    f = open('task1.out', 'w')
    f.write(str(output.hex()))
    f.close()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(usage="%(prog)s path/to/ciphertext1.txt path/to/ciphertext2.txt 'a multi word guess' 5")
    parser.add_argument('ciphertext1', help = 'path to file for ciphertext1')
    parser.add_argument('ciphertext2', help = 'path to file for ciphertext2')
    parser.add_argument('guess', help = 'A string. Note to pass in a multi word guess you need to put quotes around it ')
    parser.add_argument('offset', help = 'An integer where guess goes',type=int)
    args = parser.parse_args()
    with open(args.ciphertext1) as f1:
        with open(args.ciphertext2) as f2:
            load_and_format_run(f1,f2,args.guess,args.offset)
