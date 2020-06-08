roundor_url=https://ff.static.1001fonts.net/r/o/roundornoncommercial.regular.otf
nunito_url=https://www.1001freefonts.com/d/5900/nunito.zip
space_mono_url=https://ff.static.1001fonts.net/s/p/space-mono.regular.ttf
space_mono_bold_url=https://ff.static.1001fonts.net/s/p/space-mono.bold.ttf

wget --no-use-server-timestamps "$roundor_url" -O roundor.otf
wget --no-use-server-timestamps "$nunito_url" -O nunito.zip
unzip nunito.zip Nunito-Regular.ttf
mv Nunito-Regular.ttf nunito.ttf
rm nunito.zip
wget --no-use-server-timestamps "$space_mono_url" -O space_mono.ttf
wget --no-use-server-timestamps "$space_mono_bold_url" -O space_mono_bold.ttf
