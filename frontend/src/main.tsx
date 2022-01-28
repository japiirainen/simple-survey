import React from 'react'
import ReactDOM from 'react-dom'
import {BrowserRouter} from 'react-router-dom'

const App = () => {
  React.useEffect(() => {
    fetch('http://localhost:8081/login', {
      method: 'POST',
      body: JSON.stringify({
        loginUseName: 'joona',
        loginPassword: 'joona',
      }),
      credentials: 'same-origin',
      mode: 'cors',
      headers: {
        'Content-Type': 'application/json',
      },
    })
  })
  return (
    <div>
      <h1>Hello world</h1>
    </div>
  )
}

ReactDOM.render(
  <React.StrictMode>
    <BrowserRouter>
      <App />
    </BrowserRouter>
  </React.StrictMode>,
  document.getElementById('app')
)
