
# Bilder einfügen via upload-link

So sieht der HTML-code eines Bildes ein, das über die Mediathek geladen habt:


<img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="600" />

```
<img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="600" />

```

Falls wir Unetrtitel hinzufügen wollen, dann geht das mit folgender Ergänzung:

```
<figure>
    <img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="600" />
    <figcaption>Caption goes here</figcaption>
</figure>
```
