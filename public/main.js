// const { Elm } = require('./src/Pages/PointOfSale.elm')
const BCHJS = require("@psf/bch-js")
let bchjs = new BCHJS() // Defaults to BCHN network.

var app = Elm.Main.init({
    node: document.getElementById("elm-node")
});
app.ports.getCashAddress.subscribe(mnemonic => {
    // create mnemonic
    //let mnemonic = bchjs.Mnemonic.generate(128);
    restoreWalletFromMnenomic(mnemonic).then(cashAddress => {
        app.ports.cashAddressReceiver.send(cashAddress)

    })
    // create seed buffer from mnemonic

})

app.ports.getBchPrice.subscribe(_ => {
    bchjs.Price.getBchUsd().then(current => {
        app.ports.bchPriceReceiver.send(current)
    })
})

let restoreWalletFromMnenomic = mnemonic => {
    if (mnemonic) {
        return bchjs.Mnemonic.toSeed(mnemonic).then(seedBuffer => {
            // create HDNode from seed buffer
            let hdNode = bchjs.HDNode.fromSeed(seedBuffer);
            // derive hardened child HDNode
            let account = bchjs.HDNode.deriveHardened(hdNode, 0);
            let cashAddress = bchjs.HDNode.toCashAddress(account)
            return cashAddress
        })
    }
    return Promise.reject("No Wallet found in localStorage")
}

app.ports.getWalletFromLocalStorage.subscribe(itemName => {
    let mnemonic = localStorage.getItem('wallet');
    app.ports.mnemonicFromLocalStorageReceiver.send(mnemonic)
})


app.ports.fetchAddressBalance.subscribe(address => {
    bchjs.Electrumx.balance(address).then(balance => {
        console.log(balance)
        app.ports.addressBalanceReceiver.send(balance.balance.confirmed + balance.balance.unconfirmed)
    })
})