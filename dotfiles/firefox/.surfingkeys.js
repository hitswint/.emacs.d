map('<Alt-p>', 'E');
map('<Alt-n>', 'R');
map('<Alt-P>', '<<');
map('<Alt-N>', '>>');
map('B', 'S');
map('F', 'D');
map('<', '[[');
map('>', ']]');
vmap('<ArrowRight>', 'l');
vmap('<ArrowLeft>', 'h');
vmap('<ArrowDown>', 'j');
vmap('<ArrowUp>', 'k');
vmap('<End>', '$');
vmap('<Home>', '0');
vmap('<Ctrl-ArrowRight>', 'w');
vmap('<Ctrl-ArrowLeft>', 'b');
vmap('<Ctrl-End>', 'G');
vmap('<Ctrl-Home>', 'gg');
cmap('<PageUp>', '<Ctrl-,>');
cmap('<PageDown>', '<Ctrl-.>');

mapkey('p', "Open the clipboard's URL in the current tab", function() {
    Clipboard.read(function(response) {
        if (response.data.startsWith("http://") || response.data.startsWith("https://")) {
            window.location = response.data;
        } else {
            window.location = response.data = "https://www.google.com/search?q=" + response.data;
        }
    });
});
mapkey('P', 'Open link from clipboard', function() {
    Clipboard.read(function(response) {
        if (response.data.startsWith("http://") || response.data.startsWith("https://")) {
            tabOpenLink(response.data);
        } else {
            tabOpenLink("https://www.google.com/search?q=" + response.data);
        }
    });
});

Hints.characters = "abcdefghijklmnopqrstuvwxyz";
settings.hintAlign = "left";

settings.defaultSearchEngine = "b";
settings.focusFirstCandidate = true;
settings.omnibarMaxResults = 20;
mapkey('<Ctrl-,>', 'Choose a tab with omnibar', function() {
    Front.openOmnibar({type: "Tabs"});
});
map('<Ctrl-.>', 't');
map('<Ctrl-\'>', 'b');

addSearchAliasX('o', 'bing_cn', 'http://www.bing.com/search?q=');
mapkey('oo', '#8Open Search with alias o', function() {
    Front.openOmnibar({type: "SearchEngine", extra: "o"});
});

const quickmarks = {
    "b": ["baidu", "https://www.baidu.com"],
    "g": ["google", "https://www.google.com"],
    "o": ["bing", "https://www.bing.com"],
};
for (const key of Object.keys(quickmarks)) {
    const binding = "`" + key;
    const description = "#2 Open " + quickmarks[key][0];
    const command = function() { tabOpenLink(quickmarks[key][1]); };
    mapkey(binding, description, command);
}
