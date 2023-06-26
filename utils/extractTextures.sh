# Requires ImageMagick to be installed
for i in {0..105}
    do
        ./wolf3dextract -ext wl1 -tex "$i" | ./vga2ppm | convert /dev/stdin -rotate "90" "t$i.png"
    done

