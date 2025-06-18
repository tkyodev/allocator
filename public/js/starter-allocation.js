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
    const App = ElmApp.init(
        { node: node
        , flags: 
            { saved: saved
            , posix: Date.now()
            }
        }
    );
})();