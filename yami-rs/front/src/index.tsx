import React from 'react';
import { createRoot } from 'react-dom/client';
import { StateProvider } from './contexts/state';
import './index.css';
import reportWebVitals from './reportWebVitals';
import App from './components/App/App';

const root = createRoot(document.getElementById('root')!);
root.render(
  <React.StrictMode>
    <StateProvider>
      <App />
    </StateProvider>
  </React.StrictMode>
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
