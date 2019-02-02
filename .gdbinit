file build/kernel-i686.bin
target remote localhost:1234
break *(_start - 0xc0000000)
define df
    disas $eip, +24
end
