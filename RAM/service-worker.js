const CACHE_NAME = "elm-pwa-cache-v1";
const urlsToCache = [
    "/",
    "/index.html",
    "/elm.js",
    "/manifest.json"
];

// 📌 Устанавливаем сервис-воркер и кэшируем файлы
self.addEventListener("install", event => {
    event.waitUntil(
        caches.open(CACHE_NAME).then(cache => {
            return cache.addAll(urlsToCache);
        })
    );
});

// 📌 Перехватываем сетевые запросы и загружаем из кэша, если офлайн
self.addEventListener("fetch", event => {
    event.respondWith(
        caches.match(event.request).then(response => {
            return response || fetch(event.request);
        })
    );
});

// 📌 Удаляем старый кэш при обновлении
self.addEventListener("activate", event => {
    event.waitUntil(
        caches.keys().then(cacheNames => {
            return Promise.all(
                cacheNames.map(cacheName => {
                    if (cacheName !== CACHE_NAME) {
                        return caches.delete(cacheName);
                    }
                })
            );
        })
    );
});
