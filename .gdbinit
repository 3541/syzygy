target remote localhost:1234

define i686
    file build/kernel-i686.bin
    break *(_start - 0xc0000000)
end

define x86_64
    file build/kernel-x86_64.bin
    break _start
end

define df
    disas $eip, +24
end
