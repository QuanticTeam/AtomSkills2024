const AUTH_TOKEN_KEY = 'authToken'

export const authToken = {
  store(token: string) {
    localStorage.setItem(AUTH_TOKEN_KEY, token)
  },
  restore() {
    return localStorage.getItem(AUTH_TOKEN_KEY)
  },
  remove() {
    localStorage.removeItem(AUTH_TOKEN_KEY)
  },
}
