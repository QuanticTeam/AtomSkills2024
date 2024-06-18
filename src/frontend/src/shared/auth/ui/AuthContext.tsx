import { createContext } from 'react'
import { TokenPayload } from '~/entities'

export interface AuthContextValue {
  authenticated: boolean
  authInfo: {
    tokenPayload: TokenPayload
    user: {
      isAdmin: boolean
      isMentor: boolean
      isStudent: boolean
      userId: string
    }
  } | null
  login(authToken: string): void
  logout(): void
}

export const AuthContext = createContext({} as AuthContextValue)
