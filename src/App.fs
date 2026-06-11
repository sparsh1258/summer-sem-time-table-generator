module App

open Browser.Dom
open Fable.Core.JsInterop
open Feliz
open Feliz.UseElmish

importSideEffects "./styles.css"

Stats.recordVisit ()

[<ReactComponent>]
let Root () =
    let model, dispatch = React.useElmish (State.init, State.update)
    View.view model dispatch

let root = ReactDOM.createRoot (document.getElementById "root")
root.render (Root())
