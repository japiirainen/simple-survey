import React from 'react'
import ReactDOM from 'react-dom'
import {BrowserRouter} from 'react-router-dom'

import './style.css'

ReactDOM.render(
  <React.StrictMode>
    <BrowserRouter>
      <div>Hello world</div>
    </BrowserRouter>
  </React.StrictMode>,
  document.getElementById('app')
)
