(function() {
    const AppName = "allocation-app"
    const ElmApp = Elm.Allocation;
    
    let parentNode = document.querySelector(AppName);
    if (!parentNode) {
        parentNode = document.createElement(AppName);
        document.body.appendChild(parentNode);
    }
    const node = document.createElement('div');
    parentNode.appendChild(node);


    function getMetaContent(metaName) {
        const meta = document.querySelector(`meta[name="${metaName}"]`);
        return meta ? meta.content : "";
    }
    const App = ElmApp.init(
        { node: node
        , flags: 
            { saved: saved
            , posix: Date.now()
            , href: location.href
            , meta: allocation.meta
            , maybeConfluenceData: 
                { pageTitle: getMetaContent("ajs-page-title")
                , spaceKey: getMetaContent("ajs-space-key")
                , pageId: getMetaContent("ajs-page-id")
                , spaceName: getMetaContent("ajs-space-name")
                , pageVersion: getMetaContent("ajs-page-version")
                , parentPageId: getMetaContent("ajs-parent-page-id")
                , contentType: getMetaContent("ajs-content-type")
                , dataMacroId: dataMacroId
                }
            }
        }
    );
})();