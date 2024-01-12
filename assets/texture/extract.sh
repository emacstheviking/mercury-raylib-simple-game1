# Use xmlstarlet to convert sprite sheet XML into a CSV format
# .0 is appened to make pasting float values into a vector4f easy!
xmlstarlet sel -T -t -m "//SubTexture" -v "@name" -o "," -v "@x" -o ".0," -v "@y" -o ".0," -v "@width" -o ".0," -v "@height" -o ".0" -n sheet.xml


