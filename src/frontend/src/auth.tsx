import { ReactNode, createContext, useContext, useState } from 'react'
import { Navigate, redirect } from 'react-router-dom'

const AUTH_TOKEN_KEY = 'authToken'

function storeAuthToken(token: string) {
  localStorage.setItem(AUTH_TOKEN_KEY, token)
}

export function restoreAuthToken() {
  return localStorage.getItem(AUTH_TOKEN_KEY) ?? ''
}

export const AuthContext = createContext({
  authenticated: !!restoreAuthToken(),
  login: (token: string) => {},
  logout: () => {},
})

interface AuthContextProps {
  children: ReactNode
}

export function AuthProvider({ children }: AuthContextProps) {
  const [authToken, setAuthToken] = useState(restoreAuthToken())

  return (
    <AuthContext.Provider
      value={{
        authenticated: !!authToken,
        login(t: typeof authToken) {
          storeAuthToken(t)
          setAuthToken(t)
        },
        logout() {
          setAuthToken('')
          storeAuthToken('')
          redirect('/login')
        },
      }}
    >
      {children}
    </AuthContext.Provider>
  )
}
export function ProtectedRoute({ children }: { children: ReactNode }) {
  const { authenticated } = useContext(AuthContext)

  if (authenticated) return <div>{children}</div>

  return (
    <Navigate
      to="/login"
      replace={true}
    />
  )
}

export function PublicRoute({ children }: { children: ReactNode }) {
  const { authenticated } = useContext(AuthContext)

  if (!authenticated) return <div>{children}</div>

  return (
    <Navigate
      to="/main"
      replace={true}
    />
  )
}
