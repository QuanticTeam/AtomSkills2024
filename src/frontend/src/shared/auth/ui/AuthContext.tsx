import { createContext } from 'react'

interface AuthTokenValue {
  authenticated: boolean
  login(authToken: string): void
  logout(): void
}

export const AuthContext = createContext({} as AuthTokenValue)
