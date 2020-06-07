import { Elm } from './src/Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('root'),
});

// beans = {
//   name: 'beans',
//   weightKilos: 5,
//   price: 0.65,
//   barcode: '34kjnkjn345kjngdfkjnfsdkj',
//   description: 'Delicious red beans. Good for you.',
//   size: 'Small' | 'Medium' | 'Large',
//   color: 'red',
//   manufacturer: 'goya',
// };

// store = {
//   beans: 0.65,
//   cabbage: 1,
//   cabbageOffer: {
//     cabbagePlusThree: 1,
//   },
//   // 3 cabbage for 1 dollar
// };
