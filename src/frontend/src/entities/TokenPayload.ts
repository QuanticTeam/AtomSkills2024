import { UserRoleEnumStr } from '~/shared/auth/UserRoleEnum'

export interface TokenPayload {
  userId: string
  role: UserRoleEnumStr
  exp: number
}
